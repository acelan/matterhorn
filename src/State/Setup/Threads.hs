{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module State.Setup.Threads
  ( startUserRefreshThread
  , startTypingUsersRefreshThread
  , updateUserStatuses
  , startSubprocessLoggerThread
  , startTimezoneMonitorThread
  , maybeStartSpellChecker
  , startAsyncWorkerThread
  , startSyntaxMapLoaderThread
  , startLoggingThread
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.BChan
import           Control.Concurrent ( threadDelay, forkIO, MVar, putMVar, tryTakeMVar )
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.Delay
import           Control.Exception ( SomeException, try, finally, fromException )
import qualified Control.Monad.State.Strict as St
import qualified Data.Foldable as F
import           Data.List ( isInfixOf )
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import           Data.Time ( getCurrentTime, addUTCTime )
import           Lens.Micro.Platform ( (.=), (%=), (%~), mapped )
import           Skylighting.Loader ( loadSyntaxesFromDir )
import           System.Directory ( getTemporaryDirectory )
import           System.Exit ( ExitCode(ExitSuccess), exitFailure )
import           System.IO ( Handle, IOMode(AppendMode), stderr, hPutStr, hPutStrLn, hFlush, openFile, hClose )
import           System.IO.Temp ( openTempFile )
import           Text.Aspell ( Aspell, AspellOption(..), startAspell )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types

import           Constants
import           State.Editing ( requestSpellCheck )
import           TimeUtils ( lookupLocalTimeZone )
import           Types


updateUserStatuses :: STM.TVar (Seq UserId) -> MVar () -> Session -> IO (MH ())
updateUserStatuses usersVar lock session = do
  lockResult <- tryTakeMVar lock
  users <- STM.atomically $ STM.readTVar usersVar

  case lockResult of
      Just () | not (F.null users) -> do
          statuses <- mmGetUserStatusByIds users session `finally` putMVar lock ()
          return $ do
              forM_ statuses $ \s ->
                  setUserStatus (statusUserId s) (statusStatus s)
      Just () -> putMVar lock () >> return (return ())
      _ -> return $ return ()

startUserRefreshThread :: STM.TVar (Seq UserId) -> MVar () -> Session -> RequestChan -> IO ()
startUserRefreshThread usersVar lock session requestChan = void $ forkIO $ forever refresh
  where
      seconds = (* (1000 * 1000))
      userRefreshInterval = 30
      refresh = do
          STM.atomically $ STM.writeTChan requestChan $ do
            rs <- try $ updateUserStatuses usersVar lock session
            case rs of
              Left (_ :: SomeException) -> return (return ())
              Right upd -> return upd
          threadDelay (seconds userRefreshInterval)

-- This thread refreshes the map of typing users every second, forever,
-- to expire users who have stopped typing. Expiry time is 3 seconds.
startTypingUsersRefreshThread :: RequestChan -> IO ()
startTypingUsersRefreshThread requestChan = void $ forkIO $ forever refresh
  where
    seconds = (* (1000 * 1000))
    refreshIntervalMicros = ceiling $ seconds $ userTypingExpiryInterval / 2
    refresh = do
      STM.atomically $ STM.writeTChan requestChan $ return $ do
        now <- liftIO getCurrentTime
        let expiry = addUTCTime (- userTypingExpiryInterval) now
        let expireUsers c = c & ccInfo.cdTypingUsers %~ expireTypingUsers expiry
        csChannels . mapped %= expireUsers

      threadDelay refreshIntervalMicros

startSubprocessLoggerThread :: STM.TChan ProgramOutput -> RequestChan -> IO ()
startSubprocessLoggerThread logChan requestChan = do
    let logMonitor mPair = do
          ProgramOutput progName args out stdoutOkay err ec <-
              STM.atomically $ STM.readTChan logChan

          -- If either stdout or stderr is non-empty or there was an exit
          -- failure, log it and notify the user.
          let emptyOutput s = null s || s == "\n"

          case ec == ExitSuccess && (emptyOutput out || stdoutOkay) && emptyOutput err of
              -- the "good" case, no output and exit sucess
              True -> logMonitor mPair
              False -> do
                  (logPath, logHandle) <- case mPair of
                      Just p ->
                          return p
                      Nothing -> do
                          tmp <- getTemporaryDirectory
                          openTempFile tmp "matterhorn-subprocess.log"

                  hPutStrLn logHandle $
                      unlines [ "Program: " <> progName
                              , "Arguments: " <> show args
                              , "Exit code: " <> show ec
                              , "Stdout:"
                              , out
                              , "Stderr:"
                              , err
                              ]
                  hFlush logHandle

                  STM.atomically $ STM.writeTChan requestChan $ do
                      return $ mhError $ ProgramExecutionFailed (T.pack progName) (T.pack logPath)

                  logMonitor (Just (logPath, logHandle))

    void $ forkIO $ logMonitor Nothing

startTimezoneMonitorThread :: TimeZoneSeries -> RequestChan -> IO ()
startTimezoneMonitorThread tz requestChan = do
  -- Start the timezone monitor thread
  let timezoneMonitorSleepInterval = minutes 5
      minutes = (* (seconds 60))
      seconds = (* (1000 * 1000))
      timezoneMonitor prevTz = do
        threadDelay timezoneMonitorSleepInterval

        newTz <- lookupLocalTimeZone
        when (newTz /= prevTz) $
            STM.atomically $ STM.writeTChan requestChan $ do
                return $ timeZone .= newTz

        timezoneMonitor newTz

  void $ forkIO (timezoneMonitor tz)


maybeStartSpellChecker :: Config -> BChan MHEvent -> IO (Maybe (Aspell, IO ()))
maybeStartSpellChecker config eventQueue = do
  case configEnableAspell config of
      False -> return Nothing
      True -> do
          let aspellOpts = catMaybes [ UseDictionary <$> (configAspellDictionary config)
                                     ]
              spellCheckerTimeout = 500 * 1000 -- 500k us = 500ms
          asResult <- either (const Nothing) Just <$> startAspell aspellOpts
          case asResult of
              Nothing -> return Nothing
              Just as -> do
                  resetSCChan <- startSpellCheckerThread eventQueue spellCheckerTimeout
                  let resetSCTimer = STM.atomically $ STM.writeTChan resetSCChan ()
                  return $ Just (as, resetSCTimer)

-- Start the background spell checker delay thread.
--
-- The purpose of this thread is to postpone the spell checker query
-- while the user is actively typing and only wait until they have
-- stopped typing before bothering with a query. This is to avoid spell
-- checker queries when the editor contents are changing rapidly.
-- Avoiding such queries reduces system load and redraw frequency.
--
-- We do this by starting a thread whose job is to wait for the event
-- loop to tell it to schedule a spell check. Spell checks are scheduled
-- by writing to the channel returned by this function. The scheduler
-- thread reads from that channel and then works with another worker
-- thread as follows:
--
-- A wakeup of the main spell checker thread causes it to determine
-- whether the worker thread is already waiting on a timer. When that
-- timer goes off, a spell check will be requested. If there is already
-- an active timer that has not yet expired, the timer's expiration is
-- extended. This is the case where typing is occurring and we want to
-- continue postponing the spell check. If there is not an active timer
-- or the active timer has expired, we create a new timer and send it to
-- the worker thread for waiting.
--
-- The worker thread works by reading a timer from its queue, waiting
-- until the timer expires, and then injecting an event into the main
-- event loop to request a spell check.
startSpellCheckerThread :: BChan MHEvent
                        -- ^ The main event loop's event channel.
                        -> Int
                        -- ^ The number of microseconds to wait before
                        -- requesting a spell check.
                        -> IO (STM.TChan ())
startSpellCheckerThread eventChan spellCheckTimeout = do
  delayWakeupChan <- STM.atomically STM.newTChan
  delayWorkerChan <- STM.atomically STM.newTChan
  delVar <- STM.atomically $ STM.newTVar Nothing

  -- The delay worker actually waits on the delay to expire and then
  -- requests a spell check.
  void $ forkIO $ forever $ do
    STM.atomically $ waitDelay =<< STM.readTChan delayWorkerChan
    writeBChan eventChan (RespEvent requestSpellCheck)

  -- The delay manager waits for requests to start a delay timer and
  -- signals the worker to begin waiting.
  void $ forkIO $ forever $ do
    () <- STM.atomically $ STM.readTChan delayWakeupChan

    oldDel <- STM.atomically $ STM.readTVar delVar
    mNewDel <- case oldDel of
        Nothing -> Just <$> newDelay spellCheckTimeout
        Just del -> do
            -- It's possible that between this check for expiration and
            -- the updateDelay below, the timer will expire -- at which
            -- point this will mean that we won't extend the timer as
            -- originally desired. But that's alright, because future
            -- keystroke will trigger another timer anyway.
            expired <- tryWaitDelayIO del
            case expired of
                True -> Just <$> newDelay spellCheckTimeout
                False -> do
                    updateDelay del spellCheckTimeout
                    return Nothing

    case mNewDel of
        Nothing -> return ()
        Just newDel -> STM.atomically $ do
            STM.writeTVar delVar $ Just newDel
            STM.writeTChan delayWorkerChan newDel

  return delayWakeupChan

startSyntaxMapLoaderThread :: Config -> BChan MHEvent -> IO ()
startSyntaxMapLoaderThread config eventChan = void $ forkIO $ do
    -- Iterate over the configured syntax directories, loading syntax
    -- maps. Ensure that entries loaded in earlier directories in the
    -- sequence take precedence over entries loaded later.
    mMaps <- forM (configSyntaxDirs config) $ \dir -> do
        result <- try $ loadSyntaxesFromDir dir
        case result of
            Left (_::SomeException) -> return Nothing
            Right (Left _)          -> return Nothing
            Right (Right m)         -> return $ Just m

    let maps = catMaybes mMaps
        finalMap = foldl M.union mempty maps

    writeBChan eventChan $ RespEvent $ do
        csResources.crSyntaxMap .= finalMap

-------------------------------------------------------------------
-- Async worker thread

startAsyncWorkerThread :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
startAsyncWorkerThread c r e = void $ forkIO $ asyncWorker c r e

asyncWorker :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
asyncWorker c r e = forever $ doAsyncWork c r e

doAsyncWork :: Config -> STM.TChan (IO (MH ())) -> BChan MHEvent -> IO ()
doAsyncWork config requestChan eventChan = do
    startWork <- case configShowBackground config of
        Disabled -> return $ return ()
        Active -> do chk <- STM.atomically $ STM.tryPeekTChan requestChan
                     case chk of
                       Nothing -> do writeBChan eventChan BGIdle
                                     return $ writeBChan eventChan $ BGBusy Nothing
                       _ -> return $ return ()
        ActiveCount -> do
          chk <- STM.atomically $ do
            chanCopy <- STM.cloneTChan requestChan
            let cntMsgs = do m <- STM.tryReadTChan chanCopy
                             case m of
                               Nothing -> return 0
                               Just _ -> (1 +) <$> cntMsgs
            cntMsgs
          case chk of
            0 -> do writeBChan eventChan BGIdle
                    return (writeBChan eventChan $ BGBusy (Just 1))
            _ -> do writeBChan eventChan $ BGBusy (Just chk)
                    return $ return ()

    req <- STM.atomically $ STM.readTChan requestChan
    startWork
    res <- try req
    case res of
      Left e -> do
          when (not $ shouldIgnore e) $ do
              let err = case fromException e of
                    Nothing -> AsyncErrEvent e
                    Just mmErr -> ServerError mmErr
              writeBChan eventChan $ IEvent $ DisplayError err
      Right upd ->
          writeBChan eventChan (RespEvent upd)

-- Filter for exceptions that we don't want to report to the user,
-- probably because they are not actionable and/or contain no useful
-- information.
--
-- E.g.
-- https://github.com/matterhorn-chat/matterhorn/issues/391
shouldIgnore :: SomeException -> Bool
shouldIgnore e = "getAddrInfo" `isInfixOf` show e

data LogThreadState =
    LogThreadState { logThreadDestination :: Maybe (FilePath, Handle)
                   , logThreadEventChan :: BChan MHEvent
                   , logThreadCommandChan :: STM.TChan LogCommand
                   , logThreadMessageBuffer :: Seq.Seq LogMessage
                   }

maxLogMessageBufferSize :: Int
maxLogMessageBufferSize = 100

-- | The logging thread.
startLoggingThread :: BChan MHEvent -> STM.TChan LogCommand -> IO ()
startLoggingThread eventChan logChan = do
    let initialState = LogThreadState { logThreadDestination = Nothing
                                      , logThreadEventChan = eventChan
                                      , logThreadCommandChan = logChan
                                      , logThreadMessageBuffer = mempty
                                      }
    void $ forkIO $
        void $ St.runStateT logThreadBody initialState

logThreadBody :: St.StateT LogThreadState IO ()
logThreadBody = forever $ nextLogCommand >>= handleLogCommand

nextLogCommand :: St.StateT LogThreadState IO LogCommand
nextLogCommand = do
    chan <- St.gets logThreadCommandChan
    liftIO $ STM.atomically $ STM.readTChan chan

handleLogCommand :: LogCommand -> St.StateT LogThreadState IO ()
handleLogCommand (LogToFile newPath) = do
    now <- liftIO getCurrentTime
    oldDest <- St.gets logThreadDestination

    shouldChange <- case oldDest of
        Nothing -> return True
        Just (oldPath, oldHandle) ->
            if oldPath == newPath
            then return False
            else do
                liftIO $ do
                    hPutStrLn oldHandle $ "[" <> show now <> "] <<< Logging end >>>"
                    hClose oldHandle
                return True

    when shouldChange $ do
        result <- liftIO $ try $ openFile newPath AppendMode
        case result of
            Left (e::SomeException) -> liftIO $ do
                hPutStrLn stderr $ "Error in log thread: could not open " <> show newPath <> ": " <> show e
                exitFailure
            Right handle -> do
                St.modify $ \s -> s { logThreadDestination = Just (newPath, handle)
                                    }
                flushLogMessageBuffer handle
                liftIO $ hPutStrLn handle $ "[" <> show now <> "] <<< Logging start >>>"
handleLogCommand (LogAMessage lm) = do
    let addMessageToBuffer s =
            let newSeq = if Seq.length s >= maxLogMessageBufferSize
                         then Seq.drop 1 s
                         else s
            in newSeq Seq.|> lm

    -- Append the message to the internal buffer, maintaining the bound
    -- on the internal buffer size.
    St.modify $ \s -> s { logThreadMessageBuffer = addMessageToBuffer (logThreadMessageBuffer s)
                        }

    -- If we have an active log destination, write the message to the
    -- output file.
    dest <- St.gets logThreadDestination
    case dest of
        Nothing -> return ()
        Just (_, handle) -> liftIO $ do
            hPutLogMessage handle lm
            hFlush handle

hPutLogMessage :: Handle -> LogMessage -> IO ()
hPutLogMessage handle (LogMessage {..}) = do
    hPutStr handle $ "[" <> show logMessageTimestamp <> "] "
    hPutStr handle $ "[" <> show logMessageCategory <> "] "
    case logMessageContext of
        Nothing -> hPutStr handle "[No context] "
        Just c  -> hPutStr handle $ "[" <> show c <> "] "
    hPutStrLn handle $ T.unpack logMessageText

flushLogMessageBuffer :: Handle -> St.StateT LogThreadState IO ()
flushLogMessageBuffer handle = do
    buf <- St.gets logThreadMessageBuffer
    when (Seq.length buf > 0) $ do
        liftIO $ do
            hPutStrLn handle "<<< Log message buffer begin >>>"
            forM_ buf (hPutLogMessage handle)
            hPutStrLn handle "<<< Log message buffer end >>>"
            hFlush handle
        St.modify $ \s -> s { logThreadMessageBuffer = mempty }
