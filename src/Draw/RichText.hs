{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
module Draw.RichText
  ( MessageData(..)
  , renderRichText
  , renderMessage
  , renderText
  , renderText'
  , renderElementSeq
  , cursorSentinel
  , addEllipsis
  , findVerbatimChunk
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( (<+>), Widget, hLimit, imageL
                       , raw, render, Size(..), Widget(..)
                       )
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Skylighting as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence ( ViewL(..)
                               , ViewR(..)
                               , (<|)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting.Core as Sky

import           Network.Mattermost.Lenses ( postEditAtL, postCreateAtL )
import           Network.Mattermost.Types ( ServerTime(..) )

import           Constants ( normalChannelSigil, userSigil )
import           Themes
import           Types ( Name, HighlightSet(..) )
import           Types.Messages
import           Types.Posts
import           Types.RichText


emptyHSet :: HighlightSet
emptyHSet = HighlightSet Set.empty Set.empty mempty

omittedUsernameType :: MessageType -> Bool
omittedUsernameType = \case
  CP Join -> True
  CP Leave -> True
  CP TopicChange -> True
  _ -> False

-- Add the edit sentinel to the end of the last block in the sequence.
-- If the last block is a paragraph, append it to that paragraph.
-- Otherwise, append a new block so it appears beneath the last
-- block-level element.
addEditSentinel :: ElementData -> Seq RichTextBlock -> Seq RichTextBlock
addEditSentinel d bs =
    case viewr bs of
        EmptyR -> bs
        (rest :> b) -> rest <> appendEditSentinel d b

appendEditSentinel :: ElementData -> RichTextBlock -> Seq RichTextBlock
appendEditSentinel sentinel b =
    let s = Para (S.singleton m)
        m = Element Normal sentinel
    in case b of
        Para is -> S.singleton $ Para (is |> Element Normal ESpace |> m)
        _ -> S.fromList [b, s]

-- | A bundled structure that includes all the information necessary
-- to render a given message
data MessageData =
    MessageData { mdEditThreshold :: Maybe ServerTime
                -- ^ If specified, any messages edited before this point
                -- in time are not indicated as edited.
                , mdShowOlderEdits :: Bool
                -- ^ Indicates whether "edited" markers should be shown
                -- for old messages (i.e., ignore the mdEditThreshold
                -- value).
                , mdShowReactions :: Bool
                -- ^ Whether to render reactions.
                , mdMessage :: Message
                -- ^ The message to render.
                , mdUserName :: Maybe Text
                -- ^ The username of the message's author, if any. This
                -- is passed here rather than obtaining from the message
                -- because we need to do lookups in the ChatState to
                -- compute this, and we don't pass the ChatState into
                -- renderMessage.
                , mdParentMessage :: Maybe Message
                -- ^ The parent message of this message, if any.
                , mdParentUserName :: Maybe Text
                -- ^ The author of the parent message, if any.
                , mdThreadState :: ThreadState
                -- ^ The thread state of this message.
                , mdRenderReplyParent :: Bool
                -- ^ Whether to render the parent message.
                , mdHighlightSet :: HighlightSet
                -- ^ The highlight set to use to highlight usernames,
                -- channel names, etc.
                , mdIndentBlocks :: Bool
                -- ^ Whether to indent the message underneath the
                -- author's name (True) or just display it to the right
                -- of the author's name (False).
                , mdMessageWidthLimit :: Maybe Int
                -- ^ A width override to use to wrap non-code blocks. If
                -- unspecified, all blocks in the message will be
                -- wrapped and truncated at the width specified by the
                -- rendering context. If specified, all non-code blocks
                -- will be wrapped at this width and code blocks will be
                -- rendered using the context's width.
                , mdMyUsername :: Text
                -- ^ The username of the user running Matterhorn.
                }

-- | renderMessage performs markdown rendering of the specified message.
renderMessage :: MessageData -> Widget Name
renderMessage md@MessageData { mdMessage = msg, .. } =
    let msgUsr = case mdUserName of
          Just u -> if omittedUsernameType (msg^.mType) then Nothing else Just u
          Nothing -> Nothing
        botElem = if isBotMessage msg then B.txt "[BOT]" else B.emptyWidget
        nameElems = case msgUsr of
          Just un
            | isEmote msg ->
                [ B.withDefAttr pinnedMessageIndicatorAttr $ B.txt $ if msg^.mPinned then "[PIN]" else ""
                , B.txt $ (if msg^.mFlagged then "[!] " else "") <> "*"
                , colorUsername mdMyUsername un un
                , botElem
                , B.txt " "
                ]
            | otherwise ->
                [ B.withDefAttr pinnedMessageIndicatorAttr $ B.txt $ if msg^.mPinned then "[PIN] " else ""
                , colorUsername mdMyUsername un un
                , botElem
                , B.txt $ (if msg^.mFlagged then "[!]" else "") <> ": "
                ]
          Nothing -> []

        -- Use the editing threshold to determine whether to append an
        -- editing indication to this message.
        maybeAugment bs = case msg^.mOriginalPost of
            Nothing -> bs
            Just p ->
                if p^.postEditAtL > p^.postCreateAtL
                then case mdEditThreshold of
                    Just cutoff | p^.postEditAtL >= cutoff ->
                        addEditSentinel (EEditSentinel True) bs
                    _ -> if mdShowOlderEdits
                         then addEditSentinel (EEditSentinel False) bs
                         else bs
                else bs

        augmentedText = maybeAugment $ msg^.mText
        msgWidget =
            vBox $ (layout mdHighlightSet mdMessageWidthLimit nameElems augmentedText . viewl) augmentedText :
                   catMaybes [msgAtch, msgReac]
        replyIndent = B.Widget B.Fixed B.Fixed $ do
            ctx <- B.getContext
            -- NB: The amount subtracted here must be the total padding
            -- added below (pad 1 + vBorder)
            w <- B.render $ B.hLimit (ctx^.B.availWidthL - 2) msgWidget
            B.render $ B.vLimit (V.imageHeight $ w^.B.imageL) $
                B.padRight (B.Pad 1) B.vBorder B.<+> (B.Widget B.Fixed B.Fixed $ return w)
        msgAtch = if Seq.null (msg^.mAttachments)
          then Nothing
          else Just $ B.withDefAttr clientMessageAttr $ vBox
                 [ B.txt ("  [attached: `" <> a^.attachmentName <> "`]")
                 | a <- toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions) || (not mdShowReactions)
          then Nothing
          else let renderR e us =
                       let n = Set.size us
                       in if | n == 1    -> " [" <> e <> "]"
                             | n > 1     -> " [" <> e <> " " <> T.pack (show n) <> "]"
                             | otherwise -> ""
                   reactionMsg = Map.foldMapWithKey renderR (msg^.mReactions)
               in Just $ B.withDefAttr emojiAttr $ B.txt ("   " <> reactionMsg)
        withParent p =
            case mdThreadState of
                NoThread -> msgWidget
                InThreadShowParent -> p B.<=> replyIndent
                InThread -> replyIndent
    in if not mdRenderReplyParent
       then msgWidget
       else case msg^.mInReplyToMsg of
          NotAReply -> msgWidget
          InReplyTo _ ->
              case mdParentMessage of
                  Nothing -> withParent (B.str "[loading...]")
                  Just pm ->
                      let parentMsg = renderMessage md
                            { mdShowOlderEdits    = False
                            , mdMessage           = pm
                            , mdUserName          = mdParentUserName
                            , mdParentMessage     = Nothing
                            , mdRenderReplyParent = False
                            , mdIndentBlocks      = False
                            }
                      in withParent (addEllipsis $ B.forceAttr replyParentAttr parentMsg)

    where
        layout :: HighlightSet -> Maybe Int -> [Widget Name] -> Seq RichTextBlock
               -> ViewL RichTextBlock -> Widget Name
        layout hs w nameElems bs xs | length xs > 1     = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Blockquote {} :< _) = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (CodeBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (HTMLBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (List {} :< _)       = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Para inlns :< _)
            | F.any breakCheck inlns                    = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs _                      = nameNextToMessage hs w nameElems bs

        multiLnLayout hs w nameElems bs =
            if mdIndentBlocks
               then vBox [ hBox nameElems
                         , hBox [B.txt "  ", renderRichText mdMyUsername hs ((subtract 2) <$> w) bs]
                         ]
               else nameNextToMessage hs w nameElems bs

        nameNextToMessage hs w nameElems bs =
            Widget Fixed Fixed $ do
                nameResult <- render $ hBox nameElems
                let newW = subtract (V.imageWidth (nameResult^.imageL)) <$> w
                render $ hBox [raw (nameResult^.imageL), renderRichText mdMyUsername hs newW bs]

        breakCheck e = eData e `elem` [ELineBreak, ESoftBreak]

addEllipsis :: Widget a -> Widget a
addEllipsis w = B.Widget (B.hSize w) (B.vSize w) $ do
    ctx <- B.getContext
    let aw = ctx^.B.availWidthL
    result <- B.render w
    let withEllipsis = (B.hLimit (aw - 3) $ B.vLimit 1 $ (B.Widget B.Fixed B.Fixed $ return result)) <+>
                       B.str "..."
    if (V.imageHeight (result^.B.imageL) > 1) || (V.imageWidth (result^.B.imageL) == aw) then
        B.render withEllipsis else
        return result

-- Cursor sentinel for tracking the user's cursor position in previews.
cursorSentinel :: Char
cursorSentinel = '‸'

-- Render markdown with username highlighting
renderRichText :: Text -> HighlightSet -> Maybe Int -> Seq RichTextBlock -> Widget a
renderRichText curUser hSet w =
  B.vBox . toList . fmap (blockToWidget curUser hSet w) . addBlankLines

-- Add blank lines only between adjacent elements of the same type, to
-- save space
addBlankLines :: Seq RichTextBlock -> Seq RichTextBlock
addBlankLines = go' . viewl
  where go' EmptyL = S.empty
        go' (x :< xs) = go x (viewl xs)
        go a@Para {} (b@Para {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@Header {} (b@Header {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@Blockquote {} (b@Blockquote {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@List {} (b@List {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@CodeBlock {} (b@CodeBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@HTMLBlock {} (b@HTMLBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go x (y :< rs) = x <| go y (viewl rs)
        go x (EmptyL) = S.singleton x
        blank = Para (S.singleton (Element Normal ESpace))

-- Render text to markdown without username highlighting
renderText :: Text -> Widget a
renderText txt = renderText' "" emptyHSet txt

renderText' :: Text -> HighlightSet -> Text -> Widget a
renderText' curUser hSet t = renderRichText curUser hSet Nothing rtBs
  where rtBs = parseMarkdown t

vBox :: F.Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . toList

hBox :: F.Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . toList

header :: Int -> Widget a
header n = B.txt (T.replicate n "#")

maybeHLimit :: Maybe Int -> Widget a -> Widget a
maybeHLimit Nothing w = w
maybeHLimit (Just i) w = hLimit i w

blockToWidget :: Text -> HighlightSet -> Maybe Int -> RichTextBlock -> Widget a
blockToWidget curUser hSet w (Para is) =
    toElementChunk curUser w is hSet
blockToWidget curUser hSet w (Header n is) =
    B.withDefAttr clientHeaderAttr $
        hBox [B.padRight (B.Pad 1) $ header n, toElementChunk curUser (subtract 1 <$> w) is hSet]
blockToWidget curUser hSet w (Blockquote is) =
    maybeHLimit w $
    addQuoting (vBox $ fmap (blockToWidget curUser hSet w) is)
blockToWidget curUser hSet w (List _ l bs) =
    maybeHLimit w $
    blocksToList curUser l w bs hSet
blockToWidget _ hSet _ (CodeBlock ci tx) =
    let f = maybe rawCodeBlockToWidget (codeBlockToWidget (hSyntaxMap hSet)) mSyntax
        mSyntax = do
            lang <- codeBlockLanguage ci
            Sky.lookupSyntax lang (hSyntaxMap hSet)
    in f tx
blockToWidget _ _ w (HTMLBlock t) =
    maybeHLimit w $
    textWithCursor t
blockToWidget _ _ w (HRule) =
    maybeHLimit w $
    B.vLimit 1 (B.fill '*')

quoteChar :: Char
quoteChar = '>'

addQuoting :: B.Widget n -> B.Widget n
addQuoting w =
    B.Widget B.Fixed (B.vSize w) $ do
        ctx <- B.getContext
        childResult <- B.render $ B.hLimit (ctx^.B.availWidthL - 2) w

        let quoteBorder = B.raw $ V.charFill (ctx^.B.attrL) quoteChar 1 height
            height = V.imageHeight $ childResult^.B.imageL

        B.render $ B.hBox [ B.padRight (B.Pad 1) quoteBorder
                          , B.Widget B.Fixed B.Fixed $ return childResult
                          ]

codeBlockToWidget :: Sky.SyntaxMap -> Sky.Syntax -> Text -> Widget a
codeBlockToWidget syntaxMap syntax tx =
    let result = Sky.tokenize cfg syntax tx
        cfg = Sky.TokenizerConfig syntaxMap False
    in case result of
        Left _ -> rawCodeBlockToWidget tx
        Right tokLines ->
            let padding = B.padLeftRight 1 (B.vLimit (length tokLines) B.vBorder)
            in (B.txt $ "[" <> Sky.sName syntax <> "]") B.<=>
               (padding <+> BS.renderRawSource textWithCursor tokLines)

rawCodeBlockToWidget :: Text -> Widget a
rawCodeBlockToWidget tx =
    B.withDefAttr codeAttr $
        let padding = B.padLeftRight 1 (B.vLimit (length theLines) B.vBorder)
            theLines = expandEmpty <$> T.lines tx
            expandEmpty "" = " "
            expandEmpty s  = s
        in padding <+> (B.vBox $ textWithCursor <$> theLines)

toElementChunk :: Text -> Maybe Int -> Seq Element -> HighlightSet -> Widget a
toElementChunk curUser w es hSet = B.Widget B.Fixed B.Fixed $ do
  ctx <- B.getContext
  let width = fromMaybe (ctx^.B.availWidthL) w
      ws    = fmap (renderElementSeq curUser) (wrapLine width hSet es)
  B.render (vBox (fmap hBox ws))

blocksToList :: Text -> ListType -> Maybe Int -> Seq (Seq RichTextBlock) -> HighlightSet -> Widget a
blocksToList curUser lt w bs hSet = vBox
  [ B.txt i <+> (vBox (fmap (blockToWidget curUser hSet w) b))
  | b <- F.toList bs
  | i <- is ]
  where is = case lt of
          Bullet _ -> repeat ("• ")
          Numbered Period s ->
            [ T.pack (show (n :: Int)) <> ". " | n <- [s..] ]
          Numbered Paren s ->
            [ T.pack (show (n :: Int)) <> ") " | n <- [s..] ]

data SplitState = SplitState
  { splitChunks  :: Seq (Seq Element)
  , splitCurrCol :: Int
  }

wrapLine :: Int -> HighlightSet -> Seq Element -> Seq (Seq Element)
wrapLine maxCols hSet = splitChunks . go (SplitState (S.singleton S.empty) 0)
  where go st (viewl-> e :< es) = go st' es
          where
              HighlightSet { hUserSet = uSet, hChannelSet = cSet } = hSet

              -- Right before we check the width of the token, we see
              -- if the token is a user or channel reference. If so, we
              -- check if it is valid. If it is valid, we leave it in
              -- place; otherwise we translate it into an ordinary text
              -- element so that it does not render highlighted as a
              -- valid user or channel reference.
              newElement = e { eData = newEData }
              newEData = case eData e of
                  EUser u ->
                      if u `Set.member` uSet
                      then EUser u
                      else EText $ userSigil <> u
                  EChannel c ->
                      if c `Set.member` cSet
                      then EChannel c
                      else EText $ normalChannelSigil <> c
                  d -> d

              addHyperlink url el = setElementStyle (Hyperlink url (eStyle el)) el

              st' =
                  case newEData of
                      EHyperlink url (Just labelEs) ->
                          go st $ addHyperlink url <$> labelEs
                      EImage url (Just labelEs) ->
                          go st $ addHyperlink url <$> labelEs
                      _ ->
                          if | newEData == ESoftBreak || newEData == ELineBreak ->
                                 st { splitChunks = splitChunks st |> S.empty
                                    , splitCurrCol = 0
                                    }
                             | available >= eWidth ->
                                 st { splitChunks  = addElement newElement (splitChunks st)
                                    , splitCurrCol = splitCurrCol st + eWidth
                                    }
                             | newEData == ESpace ->
                                 st { splitChunks = splitChunks st |> S.empty
                                    , splitCurrCol = 0
                                    }
                             | otherwise ->
                                 st { splitChunks  = splitChunks st |> S.singleton newElement
                                    , splitCurrCol = eWidth
                                    }
              available = maxCols - splitCurrCol st
              eWidth = elementWidth newElement
              addElement x (viewr-> ls :> l) = ( ls |> (l |> x))
              addElement _ _ = error "[unreachable]"
        go st _                 = st

renderElementSeq :: Text -> Seq Element -> Seq (Widget a)
renderElementSeq curUser es = renderElement curUser <$> es

renderElement :: Text -> Element -> Widget a
renderElement curUser e = addStyle sty widget
    where
        sty = eStyle e
        dat = eData e
        addStyle s = case s of
                Normal                  -> id
                Emph                    -> B.withDefAttr clientEmphAttr
                Strong                  -> B.withDefAttr clientStrongAttr
                Code                    -> B.withDefAttr codeAttr
                Hyperlink (URL url) innerSty ->
                    B.hyperlink url . B.withDefAttr urlAttr .  addStyle innerSty
        rawText = B.txt . removeCursor
        widget = case dat of
            -- Cursor sentinels get parsed as individual text nodes by
            -- Cheapskate, meaning that we get a text node with exactly
            -- one character in it. That's why we compare accordingly
            -- above. In addition, since that character has no width, we
            -- have to replace it with something that has a size (such
            -- as a space) to get Brick's visibility logic to scroll the
            -- viewport to get it into view.
            EText t                      -> if t == T.singleton (cursorSentinel)
                                            then B.visible $ B.txt " "
                                            else rawText t

            ESpace                       -> B.txt " "
            ERawHtml t                   -> rawText t
            EEditSentinel recent         -> let attr = if recent
                                                       then editedRecentlyMarkingAttr
                                                       else editedMarkingAttr
                                            in B.withDefAttr attr $ B.txt editMarking
            EUser u                      -> colorUsername curUser u $ userSigil <> u
            EChannel c                   -> B.withDefAttr channelNameAttr $
                                            B.txt $ normalChannelSigil <> c
            EHyperlink (URL url) Nothing -> rawText url
            EImage (URL url) Nothing     -> rawText url
            EEmoji em                    -> B.withDefAttr emojiAttr $
                                            B.txt $ ":" <> em <> ":"

            -- Hyperlink and image nodes with labels should not appear
            -- at this point because line-wrapping should break them up
            -- into normal text node sequences with hyperlink styles
            -- attached.
            EHyperlink {}                -> rawText "(Report renderElement bug #1)"
            EImage {}                    -> rawText "(Report renderElement bug #2)"

            -- Line breaks should never need to get rendered since the
            -- line-wrapping algorithm removes them.
            ESoftBreak                   -> B.emptyWidget
            ELineBreak                   -> B.emptyWidget

textWithCursor :: Text -> Widget a
textWithCursor t
    | T.any (== cursorSentinel) t = B.visible $ B.txt $ removeCursor t
    | otherwise = B.txt t

removeCursor :: Text -> Text
removeCursor = T.filter (/= cursorSentinel)

editMarking :: Text
editMarking = "(edited)"

elementWidth :: Element -> Int
elementWidth e =
    case eData e of
        EText t                      -> B.textWidth t
        ERawHtml t                   -> B.textWidth t
        EUser t                      -> T.length userSigil + B.textWidth t
        EChannel t                   -> T.length normalChannelSigil + B.textWidth t
        EEditSentinel _              -> B.textWidth editMarking
        EImage (URL url) Nothing     -> B.textWidth url
        EImage _ (Just is)           -> sum $ elementWidth <$> is
        EHyperlink (URL url) Nothing -> B.textWidth url
        EHyperlink _ (Just is)       -> sum $ elementWidth <$> is
        EEmoji t                     -> B.textWidth t + 2
        ESpace                       -> 1
        ELineBreak                   -> 0
        ESoftBreak                   -> 0
