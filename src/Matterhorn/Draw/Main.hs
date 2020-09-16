{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Draw.Main (drawMain) where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center ( hCenter )
import           Brick.Widgets.List ( listElements )
import           Brick.Widgets.Edit ( editContentsL, renderEditor, getEditContents )
import           Control.Arrow ( (>>>) )
import           Data.Char ( isSpace, isPunctuation )
import           Data.List ( intersperse )
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Zipper ( cursorPosition, insertChar, getText, gotoEOL )
import           Data.Time.Calendar ( fromGregorian )
import           Data.Time.Clock ( UTCTime(..) )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~), (^?!), to, view, folding )

import           Network.Mattermost.Types ( ChannelId, Type(Direct, Private, Group)
                                          , ServerTime(..), UserId
                                          )


import           Matterhorn.Constants
import           Matterhorn.Draw.ChannelList ( renderChannelList, renderChannelListHeader )
import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Autocomplete
import           Matterhorn.Draw.URLList
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.MessageSelect
import           Matterhorn.State.MessageSelect
import           Matterhorn.Themes
import           Matterhorn.TimeUtils ( justAfter, justBefore )
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( parseMarkdown )
import           Matterhorn.Types.KeyEvents


previewFromInput :: Maybe MessageType -> UserId -> Text -> Maybe Message
previewFromInput _ _ s | s == T.singleton cursorSentinel = Nothing
previewFromInput overrideTy uId s =
    -- If it starts with a slash but not /me, this has no preview
    -- representation
    let isCommand = "/" `T.isPrefixOf` s
        isEmoteCmd = "/me " `T.isPrefixOf` s
        content = if isEmoteCmd
                  then T.stripStart $ T.drop 3 s
                  else s
        msgTy = fromMaybe (if isEmoteCmd then CP Emote else CP NormalPost) overrideTy
    in if isCommand && not isEmoteCmd
       then Nothing
       else Just $ Message { _mText          = parseMarkdown content
                           , _mMarkdownSource = content
                           , _mUser          = UserI False uId
                           , _mDate          = ServerTime $ UTCTime (fromGregorian 1970 1 1) 0
                           -- The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just use today's date, but
                           -- the rendering function is pure so we
                           -- can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mMessageId     = Nothing
                           , _mReactions     = mempty
                           , _mOriginalPost  = Nothing
                           , _mFlagged       = False
                           , _mPinned        = False
                           , _mChannelId     = Nothing
                           }

-- | Tokens in spell check highlighting.
data Token =
    Ignore Text
    -- ^ This bit of text is to be ignored for the purposes of
    -- spell-checking.
    | Check Text
    -- ^ This bit of text should be checked against the spell checker's
    -- misspelling list.
    deriving (Show)

drawEditorContents :: ChatState -> HighlightSet -> [Text] -> Widget Name
drawEditorContents st hs =
    let noHighlight = txt . T.unlines
    in case st^.csEditState.cedSpellChecker of
        Nothing -> noHighlight
        Just _ ->
            case S.null (st^.csEditState.cedMisspellings) of
                True -> noHighlight
                False -> doHighlightMisspellings
                           hs
                           (st^.csEditState.cedMisspellings)

-- | This function takes a set of misspellings from the spell
-- checker, the editor lines, and builds a rendering of the text with
-- misspellings highlighted.
--
-- This function processes each line of text from the editor as follows:
--
-- * Tokenize the line based on our rules for what constitutes
--   whitespace. We do this because we need to check "words" in the
--   user's input against the list of misspellings returned by the spell
--   checker. But to do this we need to ignore the same things that
--   Aspell ignores, and it ignores whitespace and lots of puncutation.
--   We also do this because once we have identified the misspellings
--   present in the input, we need to reconstruct the user's input and
--   that means preserving whitespace so that the input looks as it was
--   originally typed.
--
-- * Once we have a list of tokens -- the whitespace tokens to be
--   preserved but ignored and the tokens to be checked -- we check
--   each non-whitespace token for presence in the list of misspellings
--   reported by the checker.
--
-- * Having indicated which tokens correspond to misspelled words, we
--   then need to coallesce adjacent tokens that are of the same
--   "misspelling status", i.e., two neighboring tokens (of whitespace
--   or check type) need to be coallesced if they both correspond to
--   text that is a misspelling or if they both are NOT a misspelling.
--   We do this so that the final Brick widget is optimal in that it
--   uses a minimal number of box cells to display substrings that have
--   the same attribute.
--
-- * Finally we build a widget out of these coallesced tokens and apply
--   the misspellingAttr attribute to the misspelled tokens.
--
-- Note that since we have to come to our own conclusion about which
-- words are worth checking in the checker's output, sometimes our
-- algorithm will differ from aspell in what is considered "part of a
-- word" and what isn't. In particular, Aspell is smart about sometimes
-- noticing that "'" is an apostrophe and at other times that it is
-- a single quote as part of a quoted string. As a result there will
-- be cases where Markdown formatting characters interact poorly
-- with Aspell's checking to result in misspellings that are *not*
-- highlighted.
--
-- One way to deal with this would be to *not* parse the user's input
-- as done here, complete with all its Markdown metacharacters, but to
-- instead 1) parse the input as Markdown, 2) traverse the Markdown AST
-- and extract the words from the relevant subtrees, and 3) spell-check
-- those words. The reason we don't do it that way in the first place is
-- because 1) the user's input might not be valid markdown and 2) even
-- if we did that, we'd still have to do this tokenization operation to
-- annotate misspellings and reconstruct the user's raw input.
doHighlightMisspellings :: HighlightSet -> S.Set Text -> [Text] -> Widget Name
doHighlightMisspellings hSet misspellings contents =
    -- Traverse the input, gathering non-whitespace into tokens and
    -- checking if they appear in the misspelling collection
    let whitelist = S.union (hUserSet hSet) (hChannelSet hSet)

        handleLine t | t == "" = txt " "
        handleLine t =
            -- For annotated tokens, coallesce tokens of the same type
            -- and add attributes for misspellings.
            let mkW (Left tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else withDefAttr misspellingAttr $ txt $ getTokenText tok
                mkW (Right tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else txt s

                go :: Either Token Token -> [Either Token Token] -> [Either Token Token]
                go lst [] = [lst]
                go lst (tok:toks) =
                    case (lst, tok) of
                        (Left a, Left b)   -> go (Left $ combineTokens a b) toks
                        (Right a, Right b) -> go (Right $ combineTokens a b) toks
                        _                  -> lst : go tok toks

            in hBox $ mkW <$> (go (Right $ Ignore "") $ annotatedTokens t)

        combineTokens (Ignore a) (Ignore b) = Ignore $ a <> b
        combineTokens (Check a) (Check b) = Check $ a <> b
        combineTokens (Ignore a) (Check b) = Check $ a <> b
        combineTokens (Check a) (Ignore b) = Check $ a <> b

        getTokenText (Ignore a) = a
        getTokenText (Check a) = a

        annotatedTokens t =
            -- For every token, check on whether it is a misspelling.
            -- The result is Either Token Token where the Left is a
            -- misspelling and the Right is not.
            checkMisspelling <$> tokenize t (Ignore "")

        checkMisspelling t@(Ignore _) = Right t
        checkMisspelling t@(Check s) =
            if s `S.member` whitelist
            then Right t
            else if s `S.member` misspellings
                 then Left t
                 else Right t

        ignoreChar c = isSpace c || isPunctuation c || c == '`' || c == '/' ||
                       T.singleton c == userSigil || T.singleton c == normalChannelSigil

        tokenize t curTok
            | T.null t = [curTok]
            | ignoreChar $ T.head t =
                case curTok of
                    Ignore s -> tokenize (T.tail t) (Ignore $ s <> (T.singleton $ T.head t))
                    Check s -> Check s : tokenize (T.tail t) (Ignore $ T.singleton $ T.head t)
            | otherwise =
                case curTok of
                    Ignore s -> Ignore s : tokenize (T.tail t) (Check $ T.singleton $ T.head t)
                    Check s -> tokenize (T.tail t) (Check $ s <> (T.singleton $ T.head t))

    in vBox $ handleLine <$> contents

renderUserCommandBox :: ChatState -> HighlightSet -> Widget Name
renderUserCommandBox st hs =
    let prompt = txt $ case st^.csEditState.cedEditMode of
            Replying _ _ -> "reply> "
            Editing _ _  ->  "edit> "
            NewPost      ->      "> "
        inputBox = renderEditor (drawEditorContents st hs) True (st^.csEditState.cedEditor)
        curContents = getEditContents $ st^.csEditState.cedEditor
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ borderElem bsHorizontal
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        st^.csEditState.cedEditor.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   txt $ "In multi-line mode. Press " <> multiLineToggleKey <>
                         " to finish."
                 ]

        replyDisplay = case st^.csEditState.cedEditMode of
            Replying msg _ ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage MessageData
                          { mdMessage           = msgWithoutParent
                          , mdUserName          = msgWithoutParent^.mUser.to (nameForUserRef st)
                          , mdParentMessage     = Nothing
                          , mdParentUserName    = Nothing
                          , mdHighlightSet      = hs
                          , mdEditThreshold     = Nothing
                          , mdShowOlderEdits    = False
                          , mdRenderReplyParent = True
                          , mdIndentBlocks      = False
                          , mdThreadState       = NoThread
                          , mdShowReactions     = True
                          , mdMessageWidthLimit = Nothing
                          , mdMyUsername        = myUsername st
                          , mdWrapNonhighlightedCodeBlocks = True
                          }
                        ]
            _ -> emptyWidget

        multiLineToggleKey = ppBinding $ getFirstDefaultBinding ToggleMultiLineEvent

        commandBox = case st^.csEditState.cedEphemeral.eesMultiline of
            False ->
                let linesStr = "line" <> if numLines == 1 then "" else "s"
                    numLines = length curContents
                in vLimit 1 $ hBox $
                   prompt : if multilineContent
                            then [ withDefAttr clientEmphAttr $
                                   str $ "[" <> show numLines <> " " <> linesStr <>
                                         "; Enter: send, " <> T.unpack multiLineToggleKey <>
                                         ": edit, Backspace: cancel] "
                                 , txt $ head curContents
                                 , showCursor MessageInput (Location (0,0)) $ str " "
                                 ]
                            else [inputBox]
            True -> vLimit multilineHeightLimit inputBox <=> multilineHints
    in replyDisplay <=> commandBox

renderChannelHeader :: ChatState -> HighlightSet -> ClientChannel -> Widget Name
renderChannelHeader st hs chan =
    let chnType = chan^.ccInfo.cdType
        topicStr = chan^.ccInfo.cdHeader
        userHeader u = let s = T.intercalate " " $ filter (not . T.null) parts
                           parts = [ userSigil <> u^.uiName
                                   , if (all T.null names)
                                     then mempty
                                     else "is"
                                   ] <> names <> [
                                     if T.null (u^.uiEmail)
                                     then mempty
                                     else "(" <> u^.uiEmail <> ")"
                                   ]
                           names = [ u^.uiFirstName
                                   , nick
                                   , u^.uiLastName
                                   ]
                           quote n = "\"" <> n <> "\""
                           nick = maybe "" quote $ u^.uiNickName
                       in s
        maybeTopic = if T.null topicStr
                     then ""
                     else " - " <> topicStr
        channelNameString = case chnType of
            Direct ->
                case chan^.ccInfo.cdDMUserId >>= flip userById st of
                    Nothing -> mkChannelName (chan^.ccInfo)
                    Just u -> userHeader u
            Private ->
                channelNamePair <> " (Private)"
            Group ->
                channelNamePair <> " (Private group)"
            _ ->
                channelNamePair
        newlineToSpace '\n' = ' '
        newlineToSpace c = c
        channelNamePair = mkChannelName (chan^.ccInfo) <> " - " <> (chan^.ccInfo.cdDisplayName)

    in renderText' (myUsername st)
         hs
         (T.map newlineToSpace (channelNameString <> maybeTopic))

renderCurrentChannelDisplay :: ChatState -> HighlightSet -> Widget Name
renderCurrentChannelDisplay st hs = header <=> messages
    where
    header =
        if st^.csShowChannelList
        then channelHeader
        else vBox [ headerWithStatus
                  , hBorder
                  ]

    headerWithStatus =
        -- Render the channel list header next to the channel header
        -- itself. We want them to be separated by a vertical border,
        -- but we want the border to be as high as the tallest of the
        -- two. To make that work we need to render the two and then
        -- render a border between them that is the same height as the
        -- taller of the two. We can't do that without making a custom
        -- widget which is why we take this approach here rather than
        -- just putting them all in an hBox.
        Widget Fixed Fixed $ do
            ctx <- getContext
            statusBox <- render $
                hLimit (configChannelListWidth $ st^.csResources.crConfiguration) $
                       (renderChannelListHeader st)

            let channelHeaderWidth = ctx^.availWidthL -
                                     (Vty.imageWidth $ statusBox^.imageL) - 1

            channelHeaderResult <- render $ hLimit channelHeaderWidth channelHeader

            let maxHeight = max (Vty.imageHeight $ statusBox^.imageL)
                                (Vty.imageHeight $ channelHeaderResult^.imageL)

            render $ hBox [ Widget Fixed Fixed $ return statusBox
                          , vLimit maxHeight vBorder
                          , Widget Fixed Fixed $ return channelHeaderResult
                          ]

    channelHeader =
        withDefAttr channelHeaderAttr $
        renderChannelHeader st hs chan

    messages = padTop Max $ padRight (Pad 1) body

    body = chatText

    chatText = case appMode st of
        MessageSelect ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        MessageSelectDeleteConfirm ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        _ ->
            cached (ChannelMessages cId) $
            renderLastMessages st hs editCutoff $
            retrogradeMsgsWithThreadStates $
            reverseMessages channelMessages

    renderMessagesWithSelect (MessageSelectState selMsgId) msgs =
        -- In this case, we want to fill the message list with messages
        -- but use the post ID as a cursor. To do this efficiently we
        -- only want to render enough messages to fill the screen.
        --
        -- If the message area is H rows high, this actually renders at
        -- most 2H rows' worth of messages and then does the appropriate
        -- cropping. This way we can simplify the math needed to figure
        -- out how to crop while bounding the number of messages we
        -- render around the cursor.
        --
        -- First, we sanity-check the application state because under
        -- some conditions, the selected message might be gone (e.g.
        -- deleted).
        let (s, (before, after)) = splitDirSeqOn (\(m, _) -> m^.mMessageId == selMsgId) msgsWithStates
            msgsWithStates = chronologicalMsgsWithThreadStates msgs
        in case s of
             Nothing ->
                 renderLastMessages st hs editCutoff before
             Just m ->
                 unsafeRenderMessageSelection (m, (before, after)) (renderSingleMessage st hs Nothing)

    cutoff = getNewMessageCutoff cId st
    editCutoff = getEditedMessageCutoff cId st
    channelMessages =
        insertTransitions (getMessageListing cId st)
                          cutoff
                          (getDateFormat st)
                          (st ^. timeZone)

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel

getMessageListing :: ChannelId -> ChatState -> Messages
getMessageListing cId st =
    st ^?! csChannels.folding (findChannelById cId) . ccContents . cdMessages . to (filterMessages isShown)
    where isShown m
            | st^.csResources.crUserPreferences.userPrefShowJoinLeave = True
            | otherwise = not $ isJoinLeave m

insertTransitions :: Messages -> Maybe NewMessageIndicator -> Text -> TimeZoneSeries -> Messages
insertTransitions ms cutoff = insertDateMarkers $ foldr addMessage ms newMessagesT
    where anyNondeletedNewMessages t =
              isJust $ findLatestUserMessage (not . view mDeleted) (messagesAfter t ms)
          newMessagesT = case cutoff of
              Nothing -> []
              Just Hide -> []
              Just (NewPostsAfterServerTime t)
                  | anyNondeletedNewMessages t -> [newMessagesMsg $ justAfter t]
                  | otherwise -> []
              Just (NewPostsStartingAt t)
                  | anyNondeletedNewMessages (justBefore t) -> [newMessagesMsg $ justBefore t]
                  | otherwise -> []
          newMessagesMsg d = newMessageOfType (T.pack "New Messages")
                             (C NewMessagesTransition) d

renderChannelSelectPrompt :: ChatState -> Widget Name
renderChannelSelectPrompt st =
    let e = st^.csChannelSelectState.channelSelectInput
    in withDefAttr channelSelectPromptAttr $
       (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
       (renderEditor (txt . T.concat) True e)

drawMain :: Bool -> ChatState -> [Widget Name]
drawMain useColor st =
    let maybeColor = if useColor then id else forceAttr "invalid"
    in maybeColor <$>
           [ connectionLayer st
           , autocompleteLayer st
           , joinBorders $ mainInterface st
           ]

connectionLayer :: ChatState -> Widget Name
connectionLayer st =
    case st^.csConnectionStatus of
        Connected -> emptyWidget
        Disconnected ->
            Widget Fixed Fixed $ do
                ctx <- getContext
                let aw = ctx^.availWidthL
                    w = length msg + 2
                    msg = "NOT CONNECTED"
                render $ translateBy (Location (max 0 (aw - w), 0)) $
                         withDefAttr errorMessageAttr $
                         border $ str msg

messageSelectBottomBar :: ChatState -> Widget Name
messageSelectBottomBar st =
    let optionList = if null usableOptions
                     then txt "(no actions available for this message)"
                     else hBox $ intersperse (txt " ") usableOptions
        usableOptions = catMaybes $ mkOption <$> options
        mkOption (f, k, desc) = if f postMsg
                                then Just $ withDefAttr messageSelectStatusAttr (txt k) <+>
                                            txt (":" <> desc)
                                else Nothing
        numURLs = Seq.length $ msgURLs postMsg
        s = if numURLs == 1 then "" else "s"
        hasURLs = numURLs > 0
        openUrlsMsg = "open " <> (T.pack $ show numURLs) <> " URL" <> s
        hasVerb = isJust (findVerbatimChunk (postMsg^.mText))
        -- make sure these keybinding pieces are up-to-date!
        ev e =
          let keyconf = st^.csResources.crConfiguration.to configUserKeys
              KeyHandlerMap keymap = messageSelectKeybindings keyconf
          in T.intercalate ","
               [ ppBinding (eventToBinding k)
               | KH { khKey     = k
                    , khHandler = h
                    } <- M.elems keymap
               , kehEventTrigger h == ByEvent e
               ]
        options = [ ( not . isGap
                    , ev YankWholeMessageEvent
                    , "yank-all"
                    )
                  , ( \m -> isFlaggable m && not (m^.mFlagged)
                    , ev FlagMessageEvent
                    , "flag"
                    )
                  , ( \m -> isFlaggable m && m^.mFlagged
                    , ev FlagMessageEvent
                    , "unflag"
                    )
                  , ( \m -> isPinnable m && not (m^.mPinned)
                    , ev PinMessageEvent
                    , "pin"
                    )
                  , ( \m -> isPinnable m && m^.mPinned
                    , ev PinMessageEvent
                    , "unpin"
                    )
                  , ( isReplyable
                    , ev ReplyMessageEvent
                    , "reply"
                    )
                  , ( not . isGap
                    , ev ViewMessageEvent
                    , "view"
                    )
                  , ( isGap
                    , ev FillGapEvent
                    , "load messages"
                    )
                  , ( \m -> isMine st m && isEditable m
                    , ev EditMessageEvent
                    , "edit"
                    )
                  , ( \m -> isMine st m && isDeletable m
                    , ev DeleteMessageEvent
                    , "delete"
                    )
                  , ( const hasURLs
                    , ev OpenMessageURLEvent
                    , openUrlsMsg
                    )
                  , ( const hasVerb
                    , ev YankMessageEvent
                    , "yank-code"
                    )
                  , ( isReactable
                    , ev ReactToMessageEvent
                    , "react"
                    )
                  ]
        Just postMsg = getSelectedMessage st

    in hBox [ borderElem bsHorizontal
            , txt "["
            , txt "Message select: "
            , optionList
            , txt "]"
            , hBorder
            ]

maybePreviewViewport :: Widget Name -> Widget Name
maybePreviewViewport w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport MessagePreviewViewport Vertical $
                         (Widget Fixed Fixed $ return result)

inputPreview :: ChatState -> HighlightSet -> Widget Name
inputPreview st hs | not $ st^.csShowMessagePreview = emptyWidget
                   | otherwise = thePreview
    where
    uId = myUserId st
    -- Insert a cursor sentinel into the input text just before
    -- rendering the preview. We use the inserted sentinel (which is
    -- not rendered) to get brick to ensure that the line the cursor is
    -- on is visible in the preview viewport. We put the sentinel at
    -- the *end* of the line because it will still influence markdown
    -- parsing and can create undesirable/confusing churn in the
    -- rendering while the cursor moves around. If the cursor is at the
    -- end of whatever line the user is editing, that is very unlikely
    -- to be a problem.
    curContents = getText $ (gotoEOL >>> insertChar cursorSentinel) $
                  st^.csEditState.cedEditor.editContentsL
    curStr = T.intercalate "\n" curContents
    overrideTy = case st^.csEditState.cedEditMode of
        Editing _ ty -> Just ty
        _ -> Nothing
    previewMsg = previewFromInput overrideTy uId curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else prview pm $ getParentMessage st pm
                     prview m p = renderMessage MessageData
                                  { mdMessage           = m
                                  , mdUserName          = m^.mUser.to (nameForUserRef st)
                                  , mdParentMessage     = p
                                  , mdParentUserName    = p >>= (^.mUser.to (nameForUserRef st))
                                  , mdHighlightSet      = hs
                                  , mdEditThreshold     = Nothing
                                  , mdShowOlderEdits    = False
                                  , mdRenderReplyParent = True
                                  , mdIndentBlocks      = True
                                  , mdThreadState       = NoThread
                                  , mdShowReactions     = True
                                  , mdMessageWidthLimit = Nothing
                                  , mdMyUsername        = myUsername st
                                  , mdWrapNonhighlightedCodeBlocks = True
                                  }
                 in (maybePreviewViewport msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: ChatState -> HighlightSet -> Widget Name
userInputArea st hs =
    case appMode st of
        ChannelSelect -> renderChannelSelectPrompt st
        UrlSelect     -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Enter"
                                        , txt " to open the selected URL or "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to cancel."
                                        ]
        MessageSelectDeleteConfirm -> renderDeleteConfirm
        _             -> renderUserCommandBox st hs

renderDeleteConfirm :: Widget Name
renderDeleteConfirm =
    hCenter $ txt "Are you sure you want to delete the selected message? (y/n)"

mainInterface :: ChatState -> Widget Name
mainInterface st =
    vBox [ if st^.csShowChannelList || appMode st == ChannelSelect
           then hBox [hLimit channelListWidth (renderChannelList st), vBorder, mainDisplay]
           else mainDisplay
         , bottomBorder
         , inputPreview st hs
         , userInputArea st hs
         ]
    where
    hs = getHighlightSet st
    channelListWidth = configChannelListWidth $ st^.csResources.crConfiguration
    mainDisplay = case appMode st of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay st hs

    bottomBorder = case appMode st of
        MessageSelect -> messageSelectBottomBar st
        _ -> maybeSubdue $ hBox
             [ showAttachmentCount
             , hBorder
             , showTypingUsers
             , showBusy
             ]

    showAttachmentCount =
        let count = length $ listElements $ st^.csEditState.cedAttachmentList
        in if count == 0
           then emptyWidget
           else hBox [ borderElem bsHorizontal
                     , withDefAttr clientMessageAttr $
                       txt $ "(" <> (T.pack $ show count) <> " attachment" <>
                             (if count == 1 then "" else "s") <> "; "
                     , withDefAttr clientEmphAttr $
                       txt $ ppBinding (getFirstDefaultBinding ShowAttachmentListEvent)
                     , txt " to manage)"
                     ]

    showTypingUsers =
        let format = renderText' (myUsername st) hs
        in case allTypingUsers (st^.csCurrentChannel.ccInfo.cdTypingUsers) of
            [] -> emptyWidget
            [uId] | Just un <- usernameForUserId uId st ->
               format $ "[" <> userSigil <> un <> " is typing]"
            [uId1, uId2] | Just un1 <- usernameForUserId uId1 st
                         , Just un2 <- usernameForUserId uId2 st ->
               format $ "[" <> userSigil <> un1 <> " and " <> userSigil <> un2 <> " are typing]"
            _ -> format "[several people are typing]"

    showBusy = case st^.csWorkerIsBusy of
                 Just (Just n) -> hLimit 2 hBorder <+> txt (T.pack $ "*" <> show n)
                 Just Nothing -> hLimit 2 hBorder <+> txt "*"
                 Nothing -> emptyWidget

    maybeSubdue = if appMode st == ChannelSelect
                  then forceAttr ""
                  else id

replyArrow :: Widget a
replyArrow =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let bs = ctx^.ctxBorderStyleL
        render $ str [' ', bsCornerTL bs, '▸']