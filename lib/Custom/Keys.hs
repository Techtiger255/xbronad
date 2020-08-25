module Custom.Keys where

-- globals
import Custom.Grid
import Custom.Prompts
import Custom.Scratch
import Custom.Vars

-- standards
import XMonad
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies, copyToAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, nextWS, prevWS, toggleWS')
import XMonad.Actions.GridSelect (goToSelected, bringSelected)
import XMonad.Actions.RotSlaves (rotAllDown, rotAllUp)
import XMonad.Actions.WithAll
-- import XMonad.Actions.Sink (sink)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Minimize

-- 
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.BoringWindows as B

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise

import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Maybe (isJust)

import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation


moverrides =
 [ (a, mnotify "usage" b) | (a, b) <-
 [ ("M-S-/",        def)
 , ("M-S-<Return>", def)
 , ("M-p",          def)
 , ("M-S-p",        off)
 , ("M-S-c",        "M-backspace")
 , ("M-S-q",        "M-delete S-delete")
 , ("M-q",          "M-delete delete")
 , ("M-<Space>",    def)
 , ("M-S-<Space>",  off)
 , ("M-n",          off)
 , ("M-<Tab>",      def)
 , ("M-S-<Tab>",    off)
 , ("M-j",          "M-r")
 , ("M-k",          "M-a")
 , ("M-m",          def)
 , ("M-<Return>",   def)
 , ("M-S-j",        "M-S-r")
 , ("M-S-k",        "M-S-a")
 , ("M-h",          def)
 , ("M-l",          off)
 , ("M-t",          "M-down *0.1")
 , ("M-,",          off)
 , ("M-.",          off)
 ]
--  ++ [ dyn k  off | k <- ['1'..'9'] ]
--  ++ [ dyn' k off | k <- ['1'..'9'] ]
 ++ [ dyn k  off | k <- "wer" ]
 ++ [ dyn' k off | k <- "wer" ]
 ]
 where
  off = "disabled"
  def = "???"
  dyn  k a = ("M-" ++ [k], a)
  dyn' k a = ("M-S-" ++ [k], a)


mkeys =
--  Xmonad
 [ ("M-<Delete> <Delete>", mnotify "xmonad" "restarting"  >> spawn "xmonad --restart")
 , ("M-<Delete> S-<Delete>", io exitSuccess)
 , ("M-l", spawn $ mlocker ++ " &")
 , ("M-0", mapM_ (\x -> unspawn x >> spawn ("sleep 1;" ++ x)) $ mtray : mrestart)  -- relaunch startup things

--  General
 , ("M-<Return>", spawn mterminal')
 , ("M-S-<Return>", spawn mterminal)
 , ("M-<Backspace>", kill1)
 , ("M-S-<Backspace>", killAll)
 , ("M-`", windows copyToAll)
 , ("M-<Escape>", killAllOtherCopies)

--  Window navigation
 , ("M-a", fUp)                 -- Focus on prev window
 , ("M-r", fDown)               -- Focus on next window
 
 , ("M-S-a", windows W.swapUp)        -- Swap with prev window
 , ("M-S-r", windows W.swapDown)      -- Swap with next window

 -- , ("M-C-a", rotAllUp)                -- Rotate all up
 -- , ("M-C-r", rotAllDown)              -- Rotate all down
 
 , ("M-z", withFocused minimizeWindow)
 , ("M-S-z", withLastMinimized maximizeWindowAndFocus)

--  Layouts
 , ("M-<Tab>", sendMessage NextLayout)
 , ("M-<D>", sinkAll) -- ?
 , ("M-<U>", withTiled' (\w -> W.float w $ W.RationalRect 0 0 1 1) >> windows W.shiftMaster) -- toggle fullscreen

--  Workspaces
 , ("M-C-<R>" , nextWS)          -- next ws
 , ("M-C-<L>"  , prevWS)          -- prev ws
 , ("M-<R>"   , safeMove Next)   -- next active ws
 , ("M-<L>"    , safeMove Prev)   -- prev active ws
 , ("M-M1-<R>", safeShift Next)  -- shift next ws
 , ("M-M1-<L>" , safeShift Prev)  -- shift prev ws
 , ("M-C-M1-<R>", withAll (\_ -> safeShift Next >> safeMove Prev) >> safeMove Next)  -- shift all to next ws
 , ("M-C-M1-<L>", withAll (\_ -> safeShift Prev >> safeMove Next) >> safeMove Prev)  -- shift all to prev ws
 , ("M-<Space>"   , toggleWS' ["NSP"])

--  Multimedia Keys
 , ("<XF86AudioPrev>" , spawn "playerctl -a previous")
 , ("<XF86AudioPlay>" , spawn "playerctl -a play-pause")
 , ("<XF86AudioPause>", spawn "playerctl -a play-pause")
 , ("<XF86AudioNext>" , spawn "playerctl -a next")
 
 , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
 , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
 , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")

 , ("<XF86MonBrightnessDown>", spawn "xbacklight -5 -time 0")
 , ("<XF86MonBrightnessUp>"  , spawn "xbacklight +5 -time 0")
 , ("M--", spawn "xbacklight -5 -time 0")
 , ("M-=", spawn "xbacklight +5 -time 0")

 , ("M-M1-<Space>", spawn "bash ~/.xmonad/kbd.sh")
 , ("<Print>", spawn mscreenshotFull)

 -- , ("M-u", sendMessage $ pullGroup U)  -- i j k l =  US, u n e i = US(COLEMAK)
 -- , ("M-n", sendMessage $ pullGroup L)
 -- , ("M-e", sendMessage $ pullGroup D)
 -- , ("M-i", sendMessage $ pullGroup R)

 -- , ("M-C-<D>", withFocused (sendMessage . MergeAll))
 -- , ("M-C-<U>", withFocused (sendMessage . UnMerge))

 -- , ("M-C-a", onGroup W.focusUp')
 -- , ("M-C-r", onGroup W.focusDown')

 -- , ("M-f <L>", withFocused $ \w -> W.float w $ W.RationalRect 0.1 0.1 0.5 1.0)
 -- , ("M-f <L>", windows $ \w -> W.float w $ W.RationalRect 0.1 0.1 0.5 1.0)
 ]  -- and M-h -> list conflicts (def in main)
--}
-- dynamic keybinds
{--}
-- like DT's dynamic keys using list comprehension, but "M-a a" & "M-a M-a"
 ++ dynamik "M" "p" [(k, namedScratchpadAction mscratch k ) | k     <- mscratch'   ]  -- scratchpads
 ++ dynamik "M" "s" [(k, S.promptSearch promptConf' f )     | (k,f) <- msearches   ]  -- search prompts
 ++                 [("M-" ++ k, f promptConf' )            | (k,f) <- mprompts    ]  -- other prompts
 ++ dynamik "M" "\\"                                                   mgrids         -- gridselects
 -- ++ dynamik "M" "f" [(k, withFocused $ \w -> W.float w $ r )    | (k,r) <- mfloatMolds ]  -- floatKeys
 where
  -- complex functions to replace *.focusUp/down
  -- adds support for tab layout to B.focusUp
  -- adds support for floating window visibility whilst switching
  fUp =
   -- onGroup W.focusUp' >>
   ifLay' "tab" (windows W.focusUp) B.focusUp
   >> ifFloat W.shiftMaster
  fDown =
   -- onGroup W.focusDown' >>
   ifLay "tab" W.focusDown
   >> ifFloat' (mapM_ ifFloat [W.swapUp, W.focusMaster]) B.focusDown


ifWS' s fn bk  = withWindowSet $ \ws -> if s `isInfixOf` (W.currentTag) ws then fn else bk
ifWS  s fn     = ifWS' s (windows fn) $ pure ()

ifLay' s fn bk = withWindowSet $ \ws -> if s `isInfixOf` (description . W.layout . W.workspace . W.current) ws then fn else bk
ifLay  s fn    = ifLay' s (windows fn) $ pure ()

ifFloat' fn bk = withWindowSet $ \ws -> case W.peek ws of Just w | M.member w $ W.floating ws -> fn; _ -> bk
ifFloat  fn    = ifFloat' (windows fn) $ pure ()


mfloatMolds =
 [ ("<L>", W.RationalRect 0.1 0.1 0.5 1.0)
 -- , ("<U>", W.RationalRect .1 .1)
 -- , ("<R>", W.RationalRect .1 .1)
 -- , ("<D>", W.RationalRect .1 .1)
 -- , ("f",   W.RationalRect .1 .1)
 ]
 where
 r = \x y w h -> W.RationalRect x y (w-(1-x)) (h-(1-y))
 c = \l t w h -> W.RationalRect (l-w) (t-h) w h


nNSP = WSIs (return (\x -> W.tag x /= "NSP"))
usednNSP = WSIs (return (\x -> isJust (W.stack x) && W.tag x /= "NSP"))
safeMove direction = moveTo direction usednNSP
safeShift direction = shiftTo direction nNSP >> safeMove direction

dynamik modif first list =
 concat $ map
  (\x -> [( mm ++ first ++ " " ++ x ++ k, f ) | (k,f) <- list ])
  ["", mm]
  where mm = modif ++ "-"

-- (~:) = (\ xs x -> xs ++ [x])

dynamik :: [Char] -> [Char] -> [([Char], X ())] -> [([Char], X ())]
moverrides :: [(String, X ())]
mkeys :: [(String, X ())]
