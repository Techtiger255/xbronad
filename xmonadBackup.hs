import Custom.Grid
import Custom.Keys
import Custom.Prompts
import Custom.Scratch
import Custom.Vars

import XMonad
import Data.Monoid
import Data.List (isInfixOf)
import Data.Char (isSpace, toLower)
import System.IO (hPutStrLn)
import XMonad.Actions.MouseResize

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, pad, xmobarPP, xmobarColor, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory


import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

--}
-- AUTOSTART
{--}

mstartupHook :: X ()
mstartupHook = do
--  spawnOnce "xmobar ~/.config/xmobar/config"  -- see main below
 spawnOnce "nitrogen --restore &"
 spawnOnce "picom &"
 spawnOnce "nm-applet &"
--  spawnOnce "volumeicon &"
 spawnOnce "trayer --edge top --widthtype request --padding 8 --SetDockType true --expand false --transparent true --tint 0x333344 --iconspacing 4 --alpha 0 --height 17 &"

--}
-- WORKSPACES
{--}
{-

-- clickable xmobar workspaces
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
 where
  doubleLts '<' = "<<"
  doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces =
 clickable . map xmobarEscape
--  $ ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
--  myWorkspaces
  where
   clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
    (i,ws) <- zip [1..9] l,
    let n = i ]

-}

mworkspaces =
 [ "work", "dev", "web", "art"
 , "sys", "etc", "virt"
 , "chat", "games"
 , "hidden" -- NSP
 ]

--}
-- MANAGEHOOK
{--}
 -- doShift "<action xdotool super+8>gfx</action>"

lower = \x -> map toLower x -- TODO: integrate into ~?
 -- https://youtrack.jetbrains.com/issue/IDEA-74679#comment=27-417315
(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (isInfixOf x) q
 -- python in keyword, ex:
 -- 'x' __ "xwindow"
 -- True ~?, False =?

 -- https://www.stackage.org/haddock/lts-16.4/xmonad-0.15/XMonad-ManageHook.html
mmanageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
mmanageHook = composeAll
 [ className =? "obs"          --> doShift ( "work" )
 
 , className ~? "jetbrains"    --> doShift ( "dev" )
 
 , title     =? "firefox"      --> doShift ( "web" )
 
 , className =? "Krita"        --> doShift ( "art" )
 , className ~? "Gimp"         --> doShift ( "art" )
 
--  , className =? "mpv"          --> doShift ( "etc" )
--  , className =? "vlc"          --> doShift ( "etc" )
 
 , className ~? "discord"      --> doShift ( "chat" )
 
 , className =? "virt-manager" --> (doFloat <+> doShift ( "virt" ))
 , className ~? "Steam"        --> (doFloat <+> doShift ( "games" ))
 
 , isDialog --> doFloat
 , isFullscreen --> doFullFloat
 ] <+> namedScratchpadManageHook mscratchPads

--}
-- LOGHOOK
{--}
mlogHook :: X ()
mlogHook = fadeInactiveLogHook 0.8

--}
-- LAYOUTS
{--}
mspacing, mspacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a

mspacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
mspacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

grid =
 renamed [Replace "grid"]
 $ limitWindows 6
 $ mspacing 2
 $ mkToggle (single MIRROR)
 $ Grid (16/10)
tabs =
 renamed [Replace "tabs"]
 $ tabbed shrinkText mtabConfig
 where
  mtabConfig =
   def
   { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
   , activeColor         = fg1
   , activeBorderColor   = bg3
   , activeTextColor     = bg1
   
   , inactiveColor       = bg1
   , inactiveBorderColor = bg2
   , inactiveTextColor   = fg1
   }

-- Theme for showWName which prints current workspace when you change workspaces.
mshowWNameTheme :: SWNConfig
mshowWNameTheme =
 def
 { swn_font              = "xft:Sans:bold:size=40"
 , swn_fade              = 1.0
 , swn_bgcolor           = bg1
 , swn_color             = fg1
 }

-- Layout hook
mlayoutHook =
 avoidStruts
 $ mouseResize
 $ windowArrange
--  $ T.toggleLayouts floats
 $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout = 
   grid
   ||| noBorders tabs

---}
-- MAIN
{--}
main :: IO ()
main = do
 xmproc0 <- spawnPipe "xmobar ~/.config/xmobar/config"
 xmonad $ ewmh def
  { manageHook =
   mmanageHook
   <+> manageDocks
  
  , handleEventHook =
   docksEventHook
   <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

  , logHook =
   workspaceHistoryHook
   <+> mlogHook
   <+> dynamicLogWithPP xmobarPP
    { ppOutput = \x -> hPutStrLn xmproc0 x

    , ppSep =  " "
    , ppUrgent          = xmobarColor ""  fg3 . pad
    , ppCurrent         = xmobarColor bg1 fg2 . pad
    , ppHidden          = xmobarColor fg1 bg3 . pad
    , ppHiddenNoWindows = xmobarColor bg1 bg2 . \( _ ) -> " "
    , ppTitle           = xmobarColor fg5 bg2 . pad
    
    , ppExtras = [windowCount]
    , ppOrder  = \(ws:l:t:ex) -> [ws] ++ ex ++ [t]
    }

  , modMask            = super
  , terminal           = mterminal
  , startupHook        = mstartupHook
  , layoutHook         = smartBorders $ (showWName' mshowWNameTheme mlayoutHook)
  , workspaces         = mworkspaces
  , borderWidth        = borderW
  , normalBorderColor  = bg3
  , focusedBorderColor = fg1
  } `additionalKeysP` mkeys
