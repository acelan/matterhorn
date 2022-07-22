{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.Command
  ( commandList
  , dispatchCommand
  , printArgSpec
  , toggleMessagePreview
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCache )
import qualified Control.Exception as Exn
import qualified Data.Char as Char
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=) )

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Exceptions as MM
import qualified Network.Mattermost.Types as MM

import           Matterhorn.State.Attachments
import           Matterhorn.Connection ( connectWebsockets )
import           Matterhorn.Constants ( normalChannelSigil )
import           Matterhorn.HelpTopics
import           Matterhorn.Scripts
import           Matterhorn.State.Help
import           Matterhorn.State.Editing
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelTopicWindow
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Common
import           Matterhorn.State.Logging
import           Matterhorn.State.PostListWindow
import           Matterhorn.State.UserListWindow
import           Matterhorn.State.ChannelListWindow
import           Matterhorn.State.ThemeListWindow
import           Matterhorn.State.Messages
import           Matterhorn.State.NotifyPrefs
import           Matterhorn.State.Teams
import           Matterhorn.State.Users
import           Matterhorn.Themes ( attrForUsername )
import           Matterhorn.Types


-- | This function skips any initial whitespace and returns the first
-- 'token' (i.e. any sequence of non-whitespace characters) as well as
-- the trailing rest of the string, after any whitespace. This is used
-- for tokenizing the first bits of command input while leaving the
-- subsequent chunks unchanged, preserving newlines and other
-- important formatting.
unwordHead :: Text -> Maybe (Text, Text)
unwordHead t =
  let t' = T.dropWhile Char.isSpace t
      (w, rs)  = T.break Char.isSpace t'
  in if T.null w
       then Nothing
       else Just (w, T.dropWhile Char.isSpace rs)

printArgSpec :: CmdArgs a -> Text
printArgSpec NoArg = ""
printArgSpec (LineArg ts) = "<" <> ts <> ">"
printArgSpec (TokenArg t NoArg) = "<" <> t <> ">"
printArgSpec (UserArg rs) = "<" <> addUserSigil "user" <> ">" <> addSpace (printArgSpec rs)
printArgSpec (ChannelArg rs) = "<" <> normalChannelSigil <> "channel>" <> addSpace (printArgSpec rs)
printArgSpec (TokenArg t rs) = "<" <> t <> ">" <> addSpace (printArgSpec rs)

addSpace :: Text -> Text
addSpace "" = ""
addSpace t = " " <> t

matchArgs :: CmdArgs a -> Text -> Either Text a
matchArgs NoArg t = case unwordHead t of
  Nothing -> return ()
  Just (a, as)
    | not (T.all Char.isSpace as) -> Left ("Unexpected arguments '" <> t <> "'")
    | otherwise -> Left ("Unexpected argument '" <> a <> "'")
matchArgs (LineArg _) t = return t
matchArgs spec@(UserArg rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as
matchArgs spec@(ChannelArg rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as
matchArgs spec@(TokenArg _ rs) t = case unwordHead t of
  Nothing -> case rs of
    NoArg -> Left ("Missing argument: " <> printArgSpec spec)
    _     -> Left ("Missing arguments: " <> printArgSpec spec)
  Just (a, as) -> (,) <$> pure a <*> matchArgs rs as

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" NoArg $ \ () -> requestQuit

  , Cmd "bye" ("對所有的人說：各位英雄後會有期") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對所有的人說：各位英雄後會有期")
  , Cmd "w" ("來，$1 喝杯高雄的自來水潤潤嗓子吧? 還有點甜呢.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("來，" <> msg <> " 喝杯高雄的自來水潤潤嗓子吧? 還有點甜呢.")
  , Cmd "ack" ("慘叫一聲, 滿臉「那也安捏?!?!?」的表情.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("慘叫一聲, 滿臉「那也安捏?!?!?」的表情.")
  , Cmd "addoil" ("對著 $1 高喊: 加油!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著 " <> msg <> " 高喊: 加油!!")
  , Cmd "agree" ("完全同意.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("完全同意.")
  , Cmd "ah" ("重重地拍了一下腦袋, 終於想到了!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("重重地拍了一下腦袋, 終於想到了!")
  , Cmd "angry" ("作出生氣的表情!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("作出生氣的表情!")
  , Cmd "applaud" ("鼓掌喝采!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("鼓掌喝采!")
  , Cmd "arc" ("挑起一邊的眉毛，古裡古怪地看著 $1。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("挑起一邊的眉毛，古裡古怪地看著 " <> msg <> "。")
  , Cmd "aluba" ("把 $1 抓去啊魯吧!魯～魯～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("把 " <> msg <> " 抓去啊魯吧!魯～魯～")
  , Cmd "badboy" ("指著 $1 說 ：你是壞人!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("指著 " <> msg <> " 說 ：你是壞人!")
  , Cmd "bite" ("狠狠的咬了 $1 一口，把他咬的哇哇大叫...真爽 真爽") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("狠狠的咬了 " <> msg <> " 一口，把他咬的哇哇大叫...真爽 真爽")
  , Cmd "blink" ("眨眨眼睛，一臉無辜的樣子。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("眨眨眼睛，一臉無辜的樣子。")
  , Cmd "blush" ("的臉紅了起來!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("的臉紅了起來!")
  , Cmd "bow" ("對著 $1 躬身作揖。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著 " <> msg <> " 躬身作揖。")
  , Cmd "bow1" ("對 $1 的景仰有如滔滔江水連綿不絕，又有如黃河氾濫一發不可收拾。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對 " <> msg <> " 的景仰有如滔滔江水連綿不絕，又有如黃河氾濫一發不可收拾。")
  , Cmd "buddha" ("在這一瞬間，覺得自己簡直跟神一樣。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("在這一瞬間，覺得自己簡直跟神一樣。")
  , Cmd "cat" ("叫了聲: 喵～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("叫了聲: 喵～")
  , Cmd "coco" ("順手拿起一把木槌，把 $1 的大頭就當木魚一樣給他.. 扣扣扣 ) ) ) ) ) ) ) 哇>！還有回音！真不素普通的『空』") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("順手拿起一把木槌，把 " <> msg <> " 的大頭就當木魚一樣給他.. 扣扣扣 ) ) ) ) ) ) ) 哇>！還有回音！真不素普通的『空』")
  , Cmd "comfort" ("對著 $1露出關愛的眼神! 說: 『 乖 $1, 惜惜! 』") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著 " <> msg <> "露出關愛的眼神! 說: 『 乖 " <> msg <> ", 惜惜! 』")
  , Cmd "cong" ("對著大家說道：恭喜恭喜！！") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著大家說道：恭喜恭喜！！")
  , Cmd "cough" ("不停地咳嗽。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("不停地咳嗽。")
  , Cmd "cow" ("對著附近的人大喊：哇勒　靠....左邊站啦～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著附近的人大喊：哇勒　靠....左邊站啦～")
  , Cmd "cower" ("怕得一直發抖。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("怕得一直發抖。")
  , Cmd "cry" ("忍不住放聲大哭. 哇嗚...") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("忍不住放聲大哭. 哇嗚...")
  , Cmd "dance" ("快樂的手足蹈舞!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("快樂的手足蹈舞!")
  , Cmd "die" ("直挺挺的倒在地上 ... 死了。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("直挺挺的倒在地上 ... 死了。")
  , Cmd "dog" ("叫了聲: 汪～汪～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("叫了聲: 汪～汪～")
  , Cmd "dunno" ("搖搖頭說道: 我不知道...") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("搖搖頭說道: 我不知道...")
  , Cmd "face" ("頑皮地做了個鬼臉:P") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("頑皮地做了個鬼臉:P")
  , Cmd "faint" ("昏倒了") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("昏倒了")
  , Cmd "fear" ("突然覺得莫名的恐懼!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("突然覺得莫名的恐懼!")
  , Cmd "flee" ("飛也似地逃了。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("飛也似地逃了。")
  , Cmd "flip" ("像瘋子一樣跳來跳去.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("像瘋子一樣跳來跳去.")
  , Cmd "flop" ("撲通地跌了一跤。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("撲通地跌了一跤。")
  , Cmd "fool" ("露出白癡般笑容對著大家笑...") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("露出白癡般笑容對著大家笑...")
  , Cmd "gg" ("為 $1 起立鼓掌!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("為 " <> msg <> " 起立鼓掌!!")
  , Cmd "giggle" ("掩著嘴吃吃地笑著。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("掩著嘴吃吃地笑著。")
  , Cmd "gold" ("唱著：『金ㄍㄠˊ金ㄍㄠˊ 出國比賽! 得冠軍，拿金牌，光榮倒鄧來！』") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("唱著：『金ㄍㄠˊ金ㄍㄠˊ 出國比賽! 得冠軍，拿金牌，光榮倒鄧來！』")
  , Cmd "gozila" ("學起酷斯拉嗚啦鳴啦的怪叫..") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("學起酷斯拉嗚啦鳴啦的怪叫..")
  , Cmd "grin" ("臉上露出邪惡的笑容。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("臉上露出邪惡的笑容。")
  , Cmd "groan" ("痛苦地呻吟著。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("痛苦地呻吟著。")
  , Cmd "gun" ("被亂槍打死了.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("被亂槍打死了.")
  , Cmd "hammer" ("舉起好大好大的鐵鎚往 $1 頭上用力一敲! *** 『 鏘 !』 ***") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("舉起好大好大的鐵鎚往 " <> msg <> " 頭上用力一敲! *** 『 鏘 !』 ***")
  , Cmd "handshake" ("和在場的每一個人握手。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("和在場的每一個人握手。")
  , Cmd "hehe" ("「嘿嘿嘿....」地奸笑了幾聲。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("「嘿嘿嘿....」地奸笑了幾聲。")
  , Cmd "hello" ("對著大家大聲喊道︰Ｈｅｌｌｏ！大家好～～：Ｄ") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著大家大聲喊道︰Ｈｅｌｌｏ！大家好～～：Ｄ")
  , Cmd "helpp" ("大叫:救命啊! 救命啊!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("大叫:救命啊! 救命啊!")
  , Cmd "hi" ("對所有的人打聲招呼 ..hi...:)") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對所有的人打聲招呼 ..hi...:)")
  , Cmd "hmm" ("「嗯」的一聲﹐一副欲言又止的樣子。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("「嗯」的一聲﹐一副欲言又止的樣子。")
  , Cmd "hoho" ("『呵..呵..呵』傻笑了幾聲。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("『呵..呵..呵』傻笑了幾聲。")
  , Cmd "hug" ("熱情擁抱 $1~^^") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("熱情擁抱 " <> msg <> "~^^")
  , Cmd "huge" ("緊緊地抱住了 $1，讓 $1 感到無比的深情。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("緊緊地抱住了 " <> msg <> "，讓 " <> msg <> " 感到無比的深情。")
  , Cmd "hungry" ("的肚子咕嚕咕嚕地叫的好大聲。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("的肚子咕嚕咕嚕地叫的好大聲。")
  , Cmd "idiot" ("突然覺得自己從沒像現在這麼愚蠢過。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("突然覺得自己從沒像現在這麼愚蠢過。")
  , Cmd "idle" ("無聊地坐在地上, 開始發起呆來了.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("無聊地坐在地上, 開始發起呆來了.")
  , Cmd "inn" ("感到十分委曲.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("感到十分委曲.")
  , Cmd "jump" ("高興地像小孩子似在 $1 四週蹦蹦跳跳.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("高興地像小孩子似在 " <> msg <> " 四週蹦蹦跳跳.")
  , Cmd "killair" ("殺氣騰騰﹐激起四周的氣流﹐產生一股狂風。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("殺氣騰騰﹐激起四周的氣流﹐產生一股狂風。")
  , Cmd "kiss" ("偷偷地在 $1 的臉頰上親了一下。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("偷偷地在 " <> msg <> " 的臉頰上親了一下。")
  , Cmd "kissy" ("激情的親著 $1 身上的每一個部位，並且訴說著自己的相思之情。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("激情的親著 " <> msg <> " 身上的每一個部位，並且訴說著自己的相思之情。")
  , Cmd "ko" ("矗立在雪蒼之頂，一臉冷漠傲視群雄，以ＫＯ每個人為己任。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("矗立在雪蒼之頂，一臉冷漠傲視群雄，以ＫＯ每個人為己任。")
  , Cmd "kok" ("舉起拳頭用力揮了揮, 看來有人要倒楣了.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("舉起拳頭用力揮了揮, 看來有人要倒楣了.")
  , Cmd "lag" ("慢慢地揮動雙手... 哇... 怎麼這麼慢!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("慢慢地揮動雙手... 哇... 怎麼這麼慢!")
  , Cmd "laugh" ("笑得倒在地上打滾。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("笑得倒在地上打滾。")
  , Cmd "lazy" ("覺得整個人懶懶的, 提不起精神來.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("覺得整個人懶懶的, 提不起精神來.")
  , Cmd "lick" ("貪婪地舔了舔 $1的臉。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("貪婪地舔了舔 " <> msg <> "的臉。")
  , Cmd "lidle" ("跳起來對每個人大喊：人家要發呆了！你們不可以趁機欺負我喔～～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("跳起來對每個人大喊：人家要發呆了！你們不可以趁機欺負我喔～～")
  , Cmd "lkiss" ("深情地看著 $1，給了 $1 淺淺卻無限情意的吻。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("深情地看著 " <> msg <> "，給了 " <> msg <> " 淺淺卻無限情意的吻。")
  , Cmd "moan" ("輕聲的呻吟著。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("輕聲的呻吟著。")
  , Cmd "nod" ("點了點頭。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("點了點頭。")
  , Cmd "ok" ("你說沒問題 ,一切有我 !!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("你說沒問題 ,一切有我 !!")
  , Cmd "ooxx" ("拿出筆在天空中迅速地畫了一沱便便!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("拿出筆在天空中迅速地畫了一沱便便!!")
  , Cmd "p" ("敲敲 $1 的頭... \"你在耍白痴ㄚ!!!\"") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("敲敲 " <> msg <> " 的頭... \"你在耍白痴ㄚ!!!\"")
  , Cmd "pa" ("正正反反地抽了 $1 幾十個耳光,把 $1 的臉打得像柿子一樣, $1 傻笑著說:\"謝您,我感覺好多了.\"") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("正正反反地抽了 " <> msg <> " 幾十個耳光,把 " <> msg <> " 的臉打得像柿子一樣, " <> msg <> " 傻笑著說:\"謝您,我感覺好多了.\"")
  , Cmd "papa" ("拍拍 $1 的背,兄弟,這好料的給你上") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("拍拍 " <> msg <> " 的背,兄弟,這好料的給你上")
  , Cmd "pat" ("輕輕地拍著 $1 的頭。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("輕輕地拍著 " <> msg <> " 的頭。")
  , Cmd "pen" ("( ._.)_ψ_") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("( ._.)_ψ_")
  , Cmd "pig" ("好豬喔 !!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("好豬喔 !!")
  , Cmd "pk" ("對著藍藍的天空鬼叫鬼叫: 我要ＰＫ～～～～ＰＫ～～～～ＰＫ～～～～") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對著藍藍的天空鬼叫鬼叫: 我要ＰＫ～～～～ＰＫ～～～～ＰＫ～～～～")
  , Cmd "point" ("伸手指著 $1。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("伸手指著 " <> msg <> "。")
  , Cmd "poke" ("很無聊地戳 $1 一下。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("很無聊地戳 " <> msg <> " 一下。")
  , Cmd "pout" ("嘟著嘴巴鬧脾氣。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("嘟著嘴巴鬧脾氣。")
  , Cmd "pp" ("霹靂啪啦的把 $1 的嘴巴打得像豬嘴巴一樣, 並說 YOU SHUT UP!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("霹靂啪啦的把 " <> msg <> " 的嘴巴打得像豬嘴巴一樣, 並說 YOU SHUT UP!")
  , Cmd "ppp" ("霹靂啪啦的把 $1 的嘴巴打得像豬嘴巴一樣, 然後貼上撒隆巴斯說 乖, 去牆角蹲著") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("霹靂啪啦的把 " <> msg <> " 的嘴巴打得像豬嘴巴一樣, 然後貼上撒隆巴斯說 乖, 去牆角蹲著")
  , Cmd "puke" ("突然覺得胃一陣抽筋, 哇啦哇啦地吐了滿地.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("突然覺得胃一陣抽筋, 哇啦哇啦地吐了滿地.")
  , Cmd "pure" ("的腦袋一片空白.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("的腦袋一片空白.")
  , Cmd "robot" ("朝天大喊 : 無敵鐵金鋼 ~~ 無敵鐵金鋼 ~~ 無 敵 鐵 金 鋼!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("朝天大喊 : 無敵鐵金鋼 ~~ 無敵鐵金鋼 ~~ 無 敵 鐵 金 鋼!!")
  , Cmd "room" ("對 $1 說：願意跟我去開房間嗎？") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對 " <> msg <> " 說：願意跟我去開房間嗎？")
  , Cmd "rose" ("手中捧著九十九朵嬌艷鮮紅的的玫瑰，對 $1 喃喃說到︰這些嬌艷的玫瑰，就像我所摯愛的他，亦象徵著我對他的愛，至死不渝。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("手中捧著九十九朵嬌艷鮮紅的的玫瑰，對 " <> msg <> " 喃喃說到︰這些嬌艷的玫瑰，就像我所摯愛的他，亦象徵著我對他的愛，至死不渝。")
  , Cmd "scowl" ("皺起眉來!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("皺起眉來!")
  , Cmd "scream" ("大聲尖叫。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("大聲尖叫。")
  , Cmd "sex" ("雙眼微張，櫻唇微開，向著大街上的人們搔首弄姿，賣弄風情。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("雙眼微張，櫻唇微開，向著大街上的人們搔首弄姿，賣弄風情。")
  , Cmd "sex1" ("將深邃的眼光望向 $1，纖纖玉手撥弄著 $1 的秀髮，看著 $0，不由得回到了舊日的時光，那倆小無猜的日子裡。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("將深邃的眼光望向 " <> msg <> "，纖纖玉手撥弄著 " <> msg <> " 的秀髮，看著 $0，不由得回到了舊日的時光，那倆小無猜的日子裡。")
  , Cmd "shake" ("緩緩地搖了搖頭.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("緩緩地搖了搖頭.")
  , Cmd "shiver" ("因寒冷而發抖。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("因寒冷而發抖。")
  , Cmd "shrug" ("無奈地聳聳肩.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("無奈地聳聳肩.")
  , Cmd "sigh" ("嘆了口氣。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("嘆了口氣。")
  , Cmd "sing" ("拉起五音不全的喉嚨哼起歌來! 「 啦 ..～ ﹏﹏」") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("拉起五音不全的喉嚨哼起歌來! 「 啦 ..～ ﹏﹏」")
  , Cmd "slap" ("狠狠摑了 $1 幾個大耳光, 打得 $1 滿天全金條, 要抓沒半條。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("狠狠摑了 " <> msg <> " 幾個大耳光, 打得 " <> msg <> " 滿天全金條, 要抓沒半條。")
  , Cmd "sleep" ("揉揉眼、打個哈欠，立刻躺倒在地上了。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("揉揉眼、打個哈欠，立刻躺倒在地上了。")
  , Cmd "slobber" ("流了滿地的口水 :D~~") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("流了滿地的口水 :D~~")
  , Cmd "sm" ("拿著蠟燭和皮鞭打量著每個人") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("拿著蠟燭和皮鞭打量著每個人")
  , Cmd "smile" ("愉快地微笑著。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("愉快地微笑著。")
  , Cmd "smirk" ("呵呵地傻笑。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("呵呵地傻笑。")
  , Cmd "snicker" ("在旁邊偷偷地笑.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("在旁邊偷偷地笑.")
  , Cmd "snort" ("哼了一聲。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("哼了一聲。")
  , Cmd "snowball" ("玩弄著手上的雪球.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("玩弄著手上的雪球.")
  , Cmd "sob" ("不停地哭哭啼啼....") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("不停地哭哭啼啼....")
  , Cmd "sorry" ("覺得很虧歉 $1!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("覺得很虧歉 " <> msg <> "!")
  , Cmd "spank" ("往 $1 的肩膀大力一拍，喊道：「老師ㄌ一ㄝ！幹得好！」") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("往 " <> msg <> " 的肩膀大力一拍，喊道：「老師ㄌ一ㄝ！幹得好！」")
  , Cmd "ssmile" ("露出陽光般燦爛的天真笑容。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("露出陽光般燦爛的天真笑容。")
  , Cmd "stare" ("用很奇怪的眼神瞄 $1。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("用很奇怪的眼神瞄 " <> msg <> "。")
  , Cmd "sweat" ("擦擦額頭上的汗水.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("擦擦額頭上的汗水.")
  , Cmd "tender" ("露出溫柔親切的表情!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("露出溫柔親切的表情!")
  , Cmd "think" ("低頭沉思.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("低頭沉思.")
  , Cmd "thx" ("對在場的所有人鞠躬道謝!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對在場的所有人鞠躬道謝!")
  , Cmd "tiger" ("對 $1 大叫：「哇哩勒？！　我聽你在虎爛～～」") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對 " <> msg <> " 大叫：「哇哩勒？！　我聽你在虎爛～～」")
  , Cmd "tsk" ("發出嘖嘖的聲音。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("發出嘖嘖的聲音。")
  , Cmd "ugly" ("對 $1 愁默默的說:本人150cm 100kg~米虫一隻 沒經濟可言, 妳願給個機會嗎?") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("對 " <> msg <> " 愁默默的說:本人150cm 100kg~米虫一隻 沒經濟可言, 妳願給個機會嗎?")
  , Cmd "unlag" ("不斷地揮動雙手大叫：哇..我變快了...天呀...怎辦.. ..") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("不斷地揮動雙手大叫：哇..我變快了...天呀...怎辦.. ..")
  , Cmd "wa" ("張大嘴, 好像看到不可思議的怪事.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("張大嘴, 好像看到不可思議的怪事.")
  , Cmd "waggle" ("搖搖食指對著 $1 說：「小朋友，這樣不可以喔！」") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("搖搖食指對著 " <> msg <> " 說：「小朋友，這樣不可以喔！」")
  , Cmd "wahaha" ("從背後拿出小旗子死命的揮舞著 ,大喊萬歲!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("從背後拿出小旗子死命的揮舞著 ,大喊萬歲!!")
  , Cmd "wake" ("揉揉眼睛，清醒了過來。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("揉揉眼睛，清醒了過來。")
  , Cmd "water" ("向著 $1 喊: 小姐水不水,多大多高, 有否佳人在旁, 給不給虧啊~~") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("向著 " <> msg <> " 喊: 小姐水不水,多大多高, 有否佳人在旁, 給不給虧啊~~")
  , Cmd "wave" ("揮揮手。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("揮揮手。")
  , Cmd "what" ("說：『ㄟ! 哩公瞎米哇奈隴聽某?？?﹖?』") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("說：『ㄟ! 哩公瞎米哇奈隴聽某?？?﹖?』")
  , Cmd "wink" ("眨眨眼!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("眨眨眼!")
  , Cmd "wolf" ("在這一瞬間, 你覺得自己簡直跟色狼一樣.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("在這一瞬間, 你覺得自己簡直跟色狼一樣.")
  , Cmd "x" ("把 $1 捉起來，拿出一根狼牙棒，對著 $1 的 (_x_) 就給他ㄨ進去...唉呀！忘了baby oil...算了 ^__^ 下次補雙份好了...") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("把 " <> msg <> " 捉起來，拿出一根狼牙棒，對著 " <> msg <> " 的 (_x_) 就給他ㄨ進去...唉呀！忘了baby oil...算了 ^__^ 下次補雙份好了...")
  , Cmd "ya" ("得意的作出勝利的手勢 V 說: 「YA」 !!!") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("得意的作出勝利的手勢 V 說: 「YA」 !!!")
  , Cmd "yawn" ("打了一個哈欠.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("打了一個哈欠.")
  , Cmd "zap" ("從天上召來一道閃電把 $1 化為一堆灰燼。") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("從天上召來一道閃電把 " <> msg <> " 化為一堆灰燼。")
  , Cmd "zzz" ("揉了揉雙眼, 打了個呵欠, 愛睏ㄍㄚˋ.") (LineArg "message") $ \msg -> do
        withCurrentTeam $ \tId ->
            execMMCommand tId "me" ("揉了揉雙眼, 打了個呵欠, 愛睏ㄍㄚˋ.")

  , Cmd "right" "Focus on the next channel" NoArg $ \ () -> do
        withCurrentTeam nextChannel

  , Cmd "left" "Focus on the previous channel" NoArg $ \ () -> do
        withCurrentTeam prevChannel

  , Cmd "create-channel" "Create a new public channel"
    (LineArg "channel name") $ \ name -> do
        withCurrentTeam $ \tId ->
            createOrdinaryChannel tId True name

  , Cmd "create-private-channel" "Create a new private channel"
    (LineArg "channel name") $ \ name -> do
        withCurrentTeam $ \tId ->
            createOrdinaryChannel tId False name

  , Cmd "delete-channel" "Delete the current channel"
    NoArg $ \ () -> do
        withCurrentTeam beginCurrentChannelDeleteConfirm

  , Cmd "hide" "Hide the current DM or group channel from the channel list"
    NoArg $ \ () -> do
        withCurrentTeam $ \tId ->
            withCurrentChannel tId $ \cId _ -> do
                hideDMChannel cId

  , Cmd "reconnect" "Force a reconnection attempt to the server"
    NoArg $ \ () ->
        connectWebsockets

  , Cmd "members" "Show the current channel's members"
    NoArg $ \ () -> do
        withCurrentTeam enterChannelMembersUserList

  , Cmd "leave" "Leave a normal channel or hide a DM channel" NoArg $ \ () -> do
        withCurrentTeam startLeaveCurrentChannel

  , Cmd "join" "Find a channel to join" NoArg $ \ () -> do
        withCurrentTeam enterChannelListWindowMode

  , Cmd "join" "Join the specified channel" (ChannelArg NoArg) $ \(n, ()) -> do
        withCurrentTeam $ \tId ->
            joinChannelByName tId n

  , Cmd "theme" "List the available themes" NoArg $ \ () -> do
        withCurrentTeam enterThemeListMode

  , Cmd "theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) -> do
        withCurrentTeam $ \tId ->
            setTheme tId themeName

  , Cmd "topic" "Set the current channel's topic (header) interactively"
    NoArg $ \ () -> do
        withCurrentTeam openChannelTopicWindow

  , Cmd "topic" "Set the current channel's topic (header)"
    (LineArg "topic") $ \ p -> do
        withCurrentTeam $ \tId ->
            if not (T.null p) then setChannelTopic tId p else return ()

  , Cmd "add-user" "Search for a user to add to the current channel"
    NoArg $ \ () -> do
        withCurrentTeam enterChannelInviteUserList

  , Cmd "msg" "Search for a user to enter a private chat"
    NoArg $ \ () -> do
        withCurrentTeam enterDMSearchUserList

  , Cmd "msg" "Chat with the specified user"
    (UserArg NoArg) $ \ (name, ()) -> do
        withCurrentTeam $ \tId ->
            changeChannelByName tId name

  , Cmd "username-attribute" "Display the attribute used to color the specified username"
    (UserArg NoArg) $ \ (name, ()) ->
        displayUsernameAttribute name

  , Cmd "msg" "Go to a user's channel and send the specified message or command"
    (UserArg $ LineArg "message or command") $ \ (name, msg) -> do
        withCurrentTeam $ \tId ->
            withFetchedUserMaybe (UserFetchByUsername name) $ \foundUser -> do
                case foundUser of
                    Just user -> createOrFocusDMChannel tId user $ Just $ \_ -> do
                        withCurrentChannel tId $ \cId _ ->
                            handleInputSubmission (channelEditor(cId)) msg
                    Nothing -> mhError $ NoSuchUser name

  , Cmd "log-start" "Begin logging debug information to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        startLogging $ T.unpack path

  , Cmd "log-snapshot" "Dump the current debug log buffer to the specified path"
    (TokenArg "path" NoArg) $ \ (path, ()) ->
        logSnapshot $ T.unpack path

  , Cmd "log-stop" "Stop logging"
    NoArg $ \ () ->
        stopLogging

  , Cmd "log-mark" "Add a custom marker message to the Matterhorn debug log"
    (LineArg "message") $ \ markMsg ->
        mhLog LogUserMark markMsg

  , Cmd "log-status" "Show current debug logging status"
    NoArg $ \ () ->
        getLogDestination

  , Cmd "add-user" "Add a user to the current channel"
    (UserArg NoArg) $ \ (uname, ()) -> do
        withCurrentTeam $ \tId ->
            addUserByNameToCurrentChannel tId uname

  , Cmd "remove" "Remove a user from the current channel"
    (UserArg NoArg) $ \ (uname, ()) -> do
        withCurrentTeam $ \tId ->
            removeUserFromCurrentChannel tId uname

  , Cmd "user" "Show users to initiate a private DM chat channel"
    -- n.b. this is identical to "msg", but is provided as an
    -- alternative mental model for useability.
    NoArg $ \ () -> do
        withCurrentTeam enterDMSearchUserList

  , Cmd "message-preview" "Toggle preview of the current message" NoArg $ \_ ->
        toggleMessagePreview

  , Cmd "toggle-truncate-verbatim-blocks" "Toggle truncation of verbatim and code blocks" NoArg $ \_ ->
        toggleVerbatimBlockTruncation

  , Cmd "toggle-channel-list" "Toggle channel list visibility" NoArg $ \_ ->
        toggleChannelListVisibility

  , Cmd "toggle-message-timestamps" "Toggle message timestamps" NoArg $ \_ ->
        toggleMessageTimestamps

  , Cmd "toggle-expanded-topics" "Toggle expanded channel topics" NoArg $ \_ ->
        toggleExpandedChannelTopics

  , Cmd "cycle-channel-list-sorting" "Cycle through channel list sorting modes for this team" NoArg $ \_ ->
        withCurrentTeam cycleChannelListSortingMode

  , Cmd "thread-orientation" "Set the orientation of the thread UI" (LineArg "left|right|above|below") $ \o ->
        setThreadOrientationByName o

  , Cmd "focus" "Focus on a channel or user"
    (ChannelArg NoArg) $ \ (name, ()) -> do
        withCurrentTeam $ \tId ->
            changeChannelByName tId name

  , Cmd "focus" "Select from available channels" NoArg $ \ () -> do
        withCurrentTeam beginChannelSelect

  , Cmd "help" "Show the main help screen" NoArg $ \ _ -> do
        withCurrentTeam $ \tId ->
            showHelpScreen tId mainHelpTopic

  , Cmd "shortcuts" "Show keyboard shortcuts" NoArg $ \ _ -> do
        withCurrentTeam $ \tId ->
            showHelpScreen tId mainHelpTopic

  , Cmd "help" "Show help about a particular topic"
      (TokenArg "topic" NoArg) $ \ (topicName, ()) -> do
          withCurrentTeam $ \tId ->
              case lookupHelpTopic topicName of
                  Nothing -> mhError $ NoSuchHelpTopic topicName
                  Just topic -> showHelpScreen tId topic

  , Cmd "sh" "List the available shell scripts" NoArg $ \ () ->
        listScripts

  , Cmd "group-create" "Create a group chat"
    (LineArg (addUserSigil "user" <> " [" <> addUserSigil "user" <> " ...]")) $ \ t -> do
        withCurrentTeam $ \tId ->
            createGroupChannel tId t

  , Cmd "sh" "Run a prewritten shell script"
    (TokenArg "script" (LineArg "message")) $ \ (script, text) -> do
        withCurrentTeam $ \tId ->
            withCurrentChannel tId $ \cId _ -> do
                findAndRunScript (channelEditor(cId)) script text

  , Cmd "flags" "Open a window of your flagged posts" NoArg $ \ () -> do
        withCurrentTeam enterFlaggedPostListMode

  , Cmd "pinned-posts" "Open a window of this channel's pinned posts" NoArg $ \ () -> do
        withCurrentTeam enterPinnedPostListMode

  , Cmd "search" "Search for posts with given terms" (LineArg "terms") $ \t -> do
        withCurrentTeam $ \tId ->
            enterSearchResultPostListMode tId t

  , Cmd "notify-prefs" "Edit the current channel's notification preferences" NoArg $ \_ -> do
        withCurrentTeam enterEditNotifyPrefsMode

  , Cmd "rename-channel-url" "Rename the current channel's URL name" (TokenArg "channel name" NoArg) $ \ (name, _) -> do
        withCurrentTeam $ \tId ->
            renameChannelUrl tId name

  , Cmd "move-team-left" "Move the currently-selected team to the left in the team list" NoArg $ \_ ->
        moveCurrentTeamLeft

  , Cmd "move-team-right" "Move the currently-selected team to the right in the team list" NoArg $ \_ ->
        moveCurrentTeamRight

  , Cmd "attach" "Attach a given file without browsing" (LineArg "path") $ \path -> do
        withCurrentTeam $ \tId -> do
            foc <- use (csTeam(tId).tsMessageInterfaceFocus)
            case foc of
                FocusThread ->
                    attachFileByPath (unsafeThreadInterface(tId)) path
                FocusCurrentChannel ->
                    withCurrentChannel tId $ \cId _ ->
                        attachFileByPath (csChannelMessageInterface(cId)) path

  , Cmd "toggle-mouse-input" "Toggle whether mouse input is enabled" NoArg $ \_ ->
        toggleMouseMode

  , Cmd "toggle-favorite" "Toggle the favorite status of the current channel" NoArg $ \_ -> do
        withCurrentTeam toggleChannelFavoriteStatus

  , Cmd "toggle-sidebar-group" "Toggle the visibility of the current channel's sidebar group" NoArg $ \_ -> do
        withCurrentTeam toggleCurrentChannelChannelListGroup

  , let names = T.intercalate "|" $ fst <$> channelListGroupNames
    in Cmd "toggle-sidebar-group" "Toggle the visibility of the named sidebar group" (LineArg names) $ \name -> do
        withCurrentTeam (toggleCurrentChannelChannelListGroupByName name)
  ]

displayUsernameAttribute :: Text -> MH ()
displayUsernameAttribute name = do
    let an = attrForUsername trimmed
        trimmed = trimUserSigil name
    postInfoMessage $ "The attribute used for " <> addUserSigil trimmed <>
                      " is " <> (attrNameToConfig an)

execMMCommand :: MM.TeamId -> Text -> Text -> MH ()
execMMCommand tId name rest = do
  withCurrentChannel tId $ \cId _ -> do
      session  <- getSession
      em       <- use (channelEditor(cId).esEditMode)
      let mc = MM.MinCommand
                 { MM.minComChannelId = cId
                 , MM.minComCommand   = "/" <> name <> " " <> rest
                 , MM.minComParentId  = case em of
                     Replying _ p -> Just $ MM.getId p
                     Editing p _  -> MM.postRootId p
                     _            -> Nothing
                 , MM.minComRootId  = case em of
                     Replying _ p -> MM.postRootId p <|> (Just $ MM.postId p)
                     Editing p _  -> MM.postRootId p
                     _            -> Nothing
                 , MM.minComTeamId = tId
                 }
          runCmd = liftIO $ do
            void $ MM.mmExecuteCommand mc session
          handleHTTP (MM.HTTPResponseException err) =
            return (Just (T.pack err))
            -- XXX: this might be a bit brittle in the future, because it
            -- assumes the shape of an error message. We might want to
            -- think about a better way of discovering this error and
            -- reporting it accordingly?
          handleCmdErr (MM.MattermostServerError err) =
            let (_, msg) = T.breakOn ": " err in
              return (Just (T.drop 2 msg))
          handleMMErr (MM.MattermostError
                         { MM.mattermostErrorMessage = msg }) =
            return (Just msg)
      errMsg <- liftIO $ (runCmd >> return Nothing) `Exn.catch` handleHTTP
                                                    `Exn.catch` handleCmdErr
                                                    `Exn.catch` handleMMErr
      case errMsg of
        Nothing -> return ()
        Just err ->
          mhError $ GenericError ("Error running command: " <> err)

dispatchCommand :: MM.TeamId -> Text -> MH ()
dispatchCommand tId cmd =
  case unwordHead cmd of
    Just (x, xs)
      | matchingCmds <- [ c
                        | c@(Cmd name _ _ _) <- commandList
                        , name == x
                        ] -> go [] matchingCmds
      where go [] [] = do
              execMMCommand tId x xs
            go errs [] = do
              let msg = ("error running command /" <> x <> ":\n" <>
                         mconcat [ "    " <> e | e <- errs ])
              mhError $ GenericError msg
            go errs (Cmd _ _ spec exe : cs) =
              case matchArgs spec xs of
                Left e -> go (e:errs) cs
                Right args -> exe args
    _ -> return ()

toggleMessagePreview :: MH ()
toggleMessagePreview = do
    mh invalidateCache
    csResources.crConfiguration.configShowMessagePreviewL %= not
