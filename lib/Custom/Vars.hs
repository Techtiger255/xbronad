module Custom.Vars where

  -- imports
import XMonad
import qualified XMonad.StackSet as W
import Data.List
import Data.Char
import Data.Traversable
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M

super = mod4Mask
alt = mod1Mask


mfont = "xft:Mononoki Nerd Font:bold:size=9"
mhome = "/home/uid0"

mbrowser = "firefox"
mfiles = "pcmanfm"

mterminal' = "termite"
mterminal = "st"
mshell = "zsh"  -- "bash"
meditor' = "micro"  -- assumes cli in scratch.hs
meditor f = mconsole' $ meditor' ++ " " ++ f
mnotify' = "notify-send"
mnotify'' = ["-i", "info"]
mnotify s c = do spawn $ joins " " [mnotify', (joins " " mnotify''), (wraps "\"" s), (wraps "\"" c)]

mbar = "/usr/bin/xmobar ~/.xmonad/xmobar.conf"  -- cabal install xmobar breaks things, but is not easily uninstalled, so the bin one is better
mtray = "trayer --edge top --widthtype request --padding 4 --SetDockType true --expand false --transparent true --tint 0x" ++ tail (bg!3) ++ " --iconspacing 4 --alpha 0 --height 17 &"
mscreenshotFull = "xfce4-screenshooter"
mlocker = "slock"
mtimeout = 5  -- in minutes


mapps =
 [ (caps mterminal, mterminal)
 , (caps mterminal', mterminal')
 , (caps mbrowser, mbrowser)
 , (caps meditor', meditor "")
 , (caps mfiles, mfiles)
 , ("Gimp",    "gimp")
 , ("OBS",     "obs")
 , ("Steam",   "steam")
 , ("Gravit",   "gravit-designer")
 ]

mconfigs =
 [(a, meditor b) | (a, b) <-
  [ ("Bashrc", "~/.bashrc")
  , ("Xmonad", "~/.xmonad/xmonad.hs")
  , ("Xmobar", "~/.config/xmobar/config")
  , ("Zshrc", "~/.zshrc")
  ]
 ]

-- ran once per session
msession =
 [ "/usr/lib/xfce-polkit/xfce-polkit"
 , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
 ]  -- mtray && mbar

-- ran on keybind and for each restart/start on every session
mrestart =
 [ "nm-applet"
 , "blueman-applet"
 , "picom"
 , "nitrogen --random --set-zoom-fill"
 ]


-- minimum of 4 colors each

bg =
 map hex6
 [ "#222"
 , "#334"
 , "#445"
 , "#556"
 ]

-- access via fg!n
fg =
 map hex6
 [ "#8bf"  -- blue
 , "#fb8"  -- orange
 , "#f88"  -- red
 , "#8fb"  -- green
 , "#b8f"  -- purple
 , "#f8b"  -- pink
 ]-- ++ hex6 "#333"

borderW = 3

fg' = prioritize fg  -- apply favorite levels
bg' = prioritize bg  -- apply favorite levels



-- characters to be appended to workspace names to apply rulesets (regular ws or fullscreen ws or floating ws)
mreg = ""
mfull = "+"
mfloat = "~"


-- customization/>


prioritize l = concat $ zipWith replicate [length l, length l-1 .. 1] l
hex6 h =
 head h : concat (
  map (\x -> replicate (if (length h') > 4 then 1 else 2) x) h'
  ) where h' = tail h
caps str = (toUpper $ head str) : tail str
joins a b = concat $ head b : [ a ++ x | x <- tail b ]
wraps a b = a ++ b ++ a
withTiled' f =
 windows $ \ws ->
  let all' = filter (not . flip M.member (W.floating ws)) . W.integrate' . W.stack . W.workspace . W.current $ ws
  in foldr f ws all'




l ! n = l!!(n-1)
q ~~? x = isInfixOf x <$> q
q ~? x = q ~~? (tail x)
valid = ['!' .. 'z'] \\ "!$&;*/<=>"

findProcess p = "pgrep -i -d ' ' " ++ reverse (takeWhile (`elem` valid) $ reverse (takeWhile (/= ' ') p))
-- based on: https://gist.github.com/altercation/fdd2dff789b4aa9287476bf33ba6167c#file-xmonad-hs-L229
unspawn p = spawn $ "kill `" ++ findProcess p ++ "` &"
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
-- focusFollowsTiledOnly :: Event -> X All
-- focusFollowsTiledOnly e@CrossingEvent {ev_window = w, ev_event_type = t}
--   | isNormalEnter = whenX bothTiled (focus w) >> mempty
--   where
--     isNormalEnter = t == enterNotify && ev_mode e == notifyNormal
--     bothTiled = notFloating w <&&> currentIsTiled
--     currentIsTiled = currentWindow >>= maybe (return True) notFloating
--     currentWindow = gets $ W.peek . windowset
--     notFloating w' = gets $ not . M.member w' . W.floating . windowset
-- focusFollowsTiledOnly _ = mempty

mconsole name exec =
 concat [mterminal', " -t ", name, " -e \"", exec, "\""]
mconsole' exec =
 concat [mterminal', " -e \"", exec, "\""]


windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
borderW :: Dimension
windowCount :: X (Maybe String)
