{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides the Drawing functionality for the
-- ChannelList sidebar.  The sidebar is divided vertically into groups
-- and each group is rendered separately.
--
-- There are actually two UI modes handled by this code:
--
--   * Normal display of the channels, with various markers to
--     indicate the current channel, channels with unread messages,
--     user state (for Direct Message channels), etc.
--
--   * ChannelSelect display where the user is typing match characters
--     into a prompt at the ChannelList sidebar is showing only those
--     channels matching the entered text (and highlighting the
--     matching portion).

module Draw.ChannelList (renderChannelList) where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center (hCenter)
import qualified Data.Text as T
import           Lens.Micro.Platform (non)

import qualified Network.Mattermost.Types as MM

import           Draw.Util
import           State.Channels
import           Themes
import           Types
import           Types.Common ( sanitizeUserText )
import qualified Zipper as Z

-- | Internal record describing each channel entry and its associated
-- attributes.  This is the object passed to the rendering function so
-- that it can determine how to render each channel.
data ChannelListEntryData =
    ChannelListEntryData { entrySigil       :: Text
                         , entryLabel       :: Text
                         , entryHasUnread   :: Bool
                         , entryMentions    :: Int
                         , entryIsRecent    :: Bool
                         , entryIsReturn    :: Bool
                         , entryIsCurrent   :: Bool
                         , entryUserStatus  :: Maybe UserStatus
                         }

renderChannelList :: ChatState -> Widget Name
renderChannelList st =
    viewport ChannelList Vertical body
    where
        cm = getColorMode st
        teamHeader = hCenter $
                     withDefAttr clientEmphAttr $
                     txt $ "Team: " <> teamNameStr
        myUsername_ = myUsername st
        me = userById (myUserId st) st
        statusSigil = maybe ' ' userSigilFromInfo me
        selfHeader = hCenter $
                     colorUsername cm myUsername_ myUsername_ (T.singleton statusSigil <> " " <> userSigil <> myUsername_)
        teamNameStr = sanitizeUserText $ MM.teamDisplayName $ st^.csMyTeam
        body = case appMode st of
            ChannelSelect ->
                let zipper = st^.csChannelSelectState.channelSelectMatches
                    matches = if Z.isEmpty zipper
                              then [hCenter $ txt "No matches"]
                              else (renderChannelListGroup st (renderChannelSelectListEntry (Z.focus zipper)) <$>
                                   Z.toList zipper)
                in vBox $
                   teamHeader :
                   selfHeader :
                   matches
            _ ->
                cached ChannelSidebar $
                vBox $
                teamHeader :
                selfHeader :
                (renderChannelListGroup st (\s e -> renderChannelListEntry cm myUsername_ $ mkChannelEntryData s e) <$>
                    Z.toList (st^.csFocus))

renderChannelListGroupHeading :: ChannelListGroup -> Bool -> Widget Name
renderChannelListGroupHeading g anyUnread =
    let label = case g of
            ChannelGroupPublicChannels -> "Public Channels"
            ChannelGroupPrivateChannels -> "Private Channels"
            ChannelGroupDirectMessages -> "Direct Messages"
        addUnread = if anyUnread
                    then (<+> (withDefAttr unreadGroupMarkerAttr $ txt "*"))
                    else id
        labelWidget = addUnread $ withDefAttr channelListHeaderAttr $ txt label
    in hBorderWithLabel labelWidget

renderChannelListGroup :: ChatState
                       -> (ChatState -> e -> (Bool, Widget Name))
                       -> (ChannelListGroup, [e])
                       -> Widget Name
renderChannelListGroup st renderEntry (group, es) =
    let heading = renderChannelListGroupHeading group anyUnread
        entryResults = renderEntry st <$> es
        (unreadFlags, entryWidgets) = unzip entryResults
        anyUnread = or unreadFlags
    in if null entryWidgets
       then emptyWidget
       else vBox (heading : entryWidgets)

mkChannelEntryData :: ChatState
                   -> ChannelListEntry
                   -> ChannelListEntryData
mkChannelEntryData st e =
    ChannelListEntryData sigilWithSpace name unread mentions recent ret current status
    where
        cId = channelListEntryChannelId e
        Just chan = findChannelById cId (st^.csChannels)
        unread = hasUnread' chan
        recent = isRecentChannel st cId
        ret = isReturnChannel st cId
        current = isCurrentChannel st cId
        (name, normalSigil, addSpace, status) = case e of
            CLChannel _ ->
                (chan^.ccInfo.cdDisplayName, Nothing, False, Nothing)
            CLGroupDM _ ->
                (chan^.ccInfo.cdDisplayName, Just " ", True, Nothing)
            CLUserDM _ uId ->
                let Just u = userById uId st
                    uname = if useNickname st
                            then u^.uiNickName.non (u^.uiName)
                            else u^.uiName
                in (uname, Just $ T.singleton $ userSigilFromInfo u, True, Just $ u^.uiStatus)
        sigilWithSpace = sigil <> if addSpace then " " else ""
        prevEditSigil = "»"
        sigil = if current
                then fromMaybe "" normalSigil
                else case chan^.ccEditState.eesInputHistoryPosition of
                    Just _ -> prevEditSigil
                    Nothing ->
                        case chan^.ccEditState.eesLastInput of
                            ("", _) -> fromMaybe "" normalSigil
                            _       -> prevEditSigil
        mentions = chan^.ccInfo.cdMentionCount

-- | Render an individual Channel List entry (in Normal mode) with
-- appropriate visual decorations.
renderChannelListEntry :: ColorMode -> Text -> ChannelListEntryData -> (Bool, Widget Name)
renderChannelListEntry cm myUName entry = (entryHasUnread entry, body)
    where
    body = decorate $ decorateEntry entry $ decorateMentions $ padRight Max $
           entryWidget $ entrySigil entry <> entryLabel entry
    decorate = if | entryIsCurrent entry ->
                      visible . forceAttr currentChannelNameAttr
                  | entryMentions entry > 0 ->
                      forceAttr mentionsChannelAttr
                  | entryHasUnread entry ->
                      forceAttr unreadChannelAttr
                  | otherwise -> id
    entryWidget = case entryUserStatus entry of
                    Just Offline -> withDefAttr clientMessageAttr . txt
                    Just _       -> colorUsername cm myUName (entryLabel entry)
                    Nothing      -> txt
    decorateMentions
      | entryMentions entry > 9 =
        (<+> str "(9+)")
      | entryMentions entry > 0 =
        (<+> str ("(" <> show (entryMentions entry) <> ")"))
      | otherwise = id

-- | Render an individual entry when in Channel Select mode,
-- highlighting the matching portion, or completely suppressing the
-- entry if it doesn't match.
renderChannelSelectListEntry :: Maybe ChannelSelectMatch -> ChatState -> ChannelSelectMatch -> (Bool, Widget Name)
renderChannelSelectListEntry curMatch st match =
    let ChannelSelectMatch preMatch inMatch postMatch _ entry = match
        maybeSelect = if (Just entry) == (matchEntry <$> curMatch)
                      then visible . withDefAttr currentChannelNameAttr
                      else id
        entryData = mkChannelEntryData st entry
    in (False, maybeSelect $
               decorateEntry entryData $
               padRight Max $
                 hBox [ txt $ entrySigil entryData <> preMatch
                      , forceAttr channelSelectMatchAttr $ txt inMatch
                      , txt postMatch
                      ])

-- If this channel is the return channel, add a decoration to denote
-- that.
--
-- Otherwise, if this channel is the most recently viewed channel (prior
-- to the currently viewed channel), add a decoration to denote that.
decorateEntry :: ChannelListEntryData -> Widget n -> Widget n
decorateEntry entry =
    if entryIsReturn entry
    then (<+> (withDefAttr recentMarkerAttr $ str returnChannelSigil))
    else if entryIsRecent entry
         then (<+> (withDefAttr recentMarkerAttr $ str recentChannelSigil))
         else id

recentChannelSigil :: String
recentChannelSigil = "<"

returnChannelSigil :: String
returnChannelSigil = "~"
