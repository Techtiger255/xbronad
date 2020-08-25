--}
-- IMPORTS
{--}

-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS')
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
-- import Data.Text (Text)
import Data.Char (isSpace, toLower)
-- import Data.CaseInsensitive (CI)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import Data.List
-- import qualified Data.Tuple.Extra as TE
import qualified Data.Map as M
import Numeric (showHex, readHex)

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
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
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

--}
-- VARIABLES
{--}
-- It's nice to assign values to stuff that you will use more than once
-- in the config. Setting values for things like font, terminal and editor
-- means you only have to change the value here to make changes globally.

-- custom :: Object
-- custom = (
--  font = "xft:Mononoki Nerd Font:bold:size=9"
--  )

super, alt :: KeyMask

super = mod4Mask
alt = mod1Mask

borderW :: Dimension
borderW = 2

myFont, myTerminal, myBrowser, myEditor, bg1, fg1, fg2 :: String

myFont = "xft:Mononoki Nerd Font:bold:size=9"

myTerminal = "st"
myBrowser = "firefox "
myEditor = myTerminal ++ " -e micro "

-- bg1 = "#077"

-- fg1 = "#6fc"

bg1 = "#222"
bg2 = "#334"
bg3 = "#556"

fg1 = "#8bf" -- blue
fg2 = "#fb8" -- orange
fg3 = "#f88" -- red
fg4 = "#f8f" -- purple
fg5 = "#8fb" -- green

-- bg' :: [Integer, Integer Integer]
-- bg' = map (showHex) (map "a")

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--}
-- AUTOSTART
{--}
-- startup :: [String]
-- startup = 
--  [ "nitrogen --restore"
--  , "xmobar ~/.config/xmobarrc"
--  , "picom"
--  , "nm-applet"
--  , "volumeicon"
--  , "trayer --edge top --widthtype request --padding 8 --SetDockType true --expand false --transparent true --tint 0x021 --iconspacing 4 --alpha 0 --height 18"
--  ]
-- coolInit :: X() -> String -> b0
-- coolInit a = do
--  spawn a ++ "&"

myStartupHook :: X ()
myStartupHook = do
--  map ([spawn a | (a,b) <- startup ) startup
--  fmap (spawn) startup
 spawnOnce "xmobar ~/.config/xmobar/config &" -- restart xmobar each time
 spawnOnce "nitrogen --restore &"
 spawnOnce "picom &"
 spawnOnce "nm-applet &"
--  spawnOnce "volumeicon &"
 spawnOnce "trayer --edge top --widthtype request --padding 8 --SetDockType true --expand false --transparent true --tint 0 --iconspacing 4 --alpha 0 --height 17 &"

--}
-- GRID SELECT
{--}
myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
 colorRangeFromClassName
  (0x11,0x22,0x33) -- lowest inactive bg
  (0x15,0x27,0x38) -- highest inactive bg
  (0x66,0xff,0xcc) -- active bg
  (0x66,0xff,0xcc) -- inactive fg
  (0x11,0x22,0x33) -- active fg
--  ("11", "22", "33")

newColors = 
 [ "112233"
 , "152738"
 , "66ffcc"
 , "66ffcc"
 , "112233"
 ]

strColorizer = stringColorizer "#fb8" True --myColorizer

-- gridSelect menu layout
-- mygridConfig :: p -> GSConfig Window
mygridConfig :: (a -> Bool -> X (String, String)) -> GSConfig a
mygridConfig colorizer = --undefined
 (buildDefaultGSConfig colorizer)
  { gs_cellheight   = 50
  , gs_cellwidth    = 75
  , gs_cellpadding  = 7
  , gs_originFractX = 0.5
  , gs_originFractY = 0
  , gs_font         = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
 where
  conf = def
   mygridConfig $ strColorizer

myApps, myConfigs :: [(String, String)]

myApps =
 [ ("Firefox", "firefox")
 , ("Kate",    "kate")
 , ("Gimp",    "gimp")
 , ("OBS",     "obs")
 , ("Dolphin", "dolphin")
 , ("Konsole", "konsole")
 , ("Steam",   "steam")
 ]
myConfigs' =
 [ ("bashrc", "~/.bashrc")
 , ("xmonad", "~/.xmonad/xmonad.hs")
 , ("xmobar", "~/.config/xmobar/config")
 , ("zshrc", "~/.zshrc")
 ]

myConfigs = [(a, myEditor ++ b) | (a, b) <- myConfigs']

-- myApps' = [ (a,b) | (a,b,c) <- myApps]  -- here for reference

--}
-- XPROMPT SETTINGS
{--}
promptConf, promptConf' :: XPConfig

promptConf =
 def
  { font                = myFont
  , bgColor             = bg1
  , fgColor             = fg1
  , bgHLight            = bg1
  , fgHLight            = fg2
  , borderColor         = bg3
  , promptBorderWidth   = 2
  , promptKeymap        = promptKeys
  , position            = CenteredAt { xpCenterY = 0.025, xpWidth = 1 } -- Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , autoComplete        = Just 250000 -- .25s  -- 10^5 -> .1s
  , showCompletionOnTab = True
  , searchPredicate     = fuzzyMatch -- isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
  }
promptConf' = promptConf { autoComplete = Nothing }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList =
 [ ("m", manPrompt)          -- manpages prompt
 , ("s", sshPrompt)          -- ssh prompt
 , ("x", xmonadPrompt)       -- xmonad prompt
 ] :: [(String, XPConfig -> X ())]

--}
-- XPROMPT KEYMAP (emacs-like key bindings for xprompts)
{--}
promptKeys :: M.Map (KeyMask,KeySym) (XP ())
promptKeys = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_v, pasteString) ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

--}
-- SEARCH ENGINES
{--}
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
news, reddit, urban, define :: S.SearchEngine

news   = S.searchEngine "news" "duckduckgo.com/?ia=news&iar=news&q="
reddit = S.searchEngine "reddit" "reddit.com/search/?q="
urban  = S.searchEngine "slang" "urbandictionary.com/define.php?term="
define = S.searchEngine "term" "dictionary.com/browse/"

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList =
 [ ("<Return>", S.duckduckgo)
 , ("h", S.stackage)
 , ("w", S.wikipedia)
 , ("y", S.youtube)
 
 , ("n", news)
 , ("r", reddit)
 , ("u", urban)
 , ("d", define)
 ]


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

myWorkspaces :: Forest String
myWorkspaces =
 [ Node "work" []
 , Node "dev" []
 , Node "web" []
 , Node "art" []
 , Node "sys" []
 , Node "etc" []
 , Node "virt" []
 , Node "chat" []
 , Node "games" []
 , Node "hidden" []
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
 -- 'abc' ~? 'abcwindow' => True
 -- 'abc' =? 'abcwindow' => False

 -- https://www.stackage.org/haddock/lts-16.4/xmonad-0.15/XMonad-ManageHook.html
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
 [ className =? "obs"          --> doShift ( "work" )
 
 , className ~? "jetbrains"    --> doShift ( "dev" )
 
 , title     =? "firefox"      --> doShift ( "web" )
 
 , className =? "Krita"        --> doShift ( "art" )
 
--  , className =? "mpv"          --> doShift ( "etc" )
--  , className =? "vlc"          --> doShift ( "etc" )
 
 , className ~? "discord"      --> doShift ( "chat" )
 
 , className ~? "Gimp"         --> (doFloat <+> doShift ( "art" ))
 , className =? "virt-manager" --> (doFloat <+> doShift ( "virt" ))
 , className ~? "Steam"        --> (doFloat <+> doShift ( "games" ))

 , title     ~? "Progress"     --> doFloat
 , stringProperty "windowRole" ~? "dialog"       --> doFloat  -- compiles fine, but doesn't work
 , className ~? "Zenity"       --> doFloat
 , className ~? "Control"      --> doFloat
 , className ~? "Toolkit"      --> doFloat
 , resource  ~? "Dialog"       --> doFloat
 ] <+> namedScratchpadManageHook myScratchPads

--}
-- LOGHOOK
{--}
myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.8

--}
-- LAYOUTS
{--}
mySpacing, mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

monocle =
 renamed [Replace "monocle"]
 $ limitWindows 20 Full
floats =
 renamed [Replace "floats"]
 $ mySpacing 5
 $ limitWindows 20 simplestFloat
grid =
 renamed [Replace "grid"]
 $ limitWindows 12
 $ mySpacing 2
 $ mkToggle (single MIRROR)
 $ Grid (16/10)
tabs =
 renamed [Replace "tabs"]
 $ tabbed shrinkText myTabConfig
 where
  myTabConfig =
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
myShowWNameTheme :: SWNConfig
myShowWNameTheme =
 def
 { swn_font              = "xft:Sans:bold:size=60"
 , swn_fade              = 1.0
 , swn_bgcolor           = bg1
 , swn_color             = fg1
 }

-- Layout hook
myLayoutHook =
 avoidStruts
 $ mouseResize
 $ windowArrange
 $ T.toggleLayouts floats
 $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
 where
  myDefaultLayout = 
   grid
   ||| noBorders monocle
   ||| noBorders tabs
   -- ||| floats

---}
-- SCRATCHPADS
{--}
myScratchPads :: [NamedScratchpad]
myScratchPads =
 [ NS "terminal" t tF dC
 , NS "micro" n nF dC
 , NS "spotify" m mF dC
 , NS "dolphin" f fF dC
 ]
 where
  dC = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 -h
    l = 0.95 -w

  t  = myTerminal ++ " -n scratcht"
  tF = resource =? "scratcht"

  n  = myTerminal ++ " -n scratchn 'micro'"
  nF = resource =? "scratchn"

  m  = "spotify"
  mF = resource =? "spotify"

  f  = "dolphin"
  fF = className =? "dolphin"

myScratchPads' :: [(String, String)]
myScratchPads' =
 [ ("t", "terminal")
 , ("n", "micro")
 , ("m", "spotify")
 , ("f", "dolphin")
 ]

 
-- oi :: (String, String)
-- oi = 
---}
-- KEYBINDINGS
{--}
myKeys :: [(String, X ())]
myKeys =
--  Xmonad
 [ ("M-q a", spawn "xmonad --recompile && xmonad --restart")
 , ("M-q c", spawn "xmonad --recompile")
 , ("M-q r", spawn "xmonad --restart")
 , ("M-q k", io exitSuccess)

--  Terminal / prompts
 , ("M-<Return>", spawn myTerminal)
 , ("M-S-<Return>", shellPrompt promptConf) -- Shell Prompt

--  Windows
 , ("M-x", kill1)
 , ("M-S-x", killAll)

--  Floating windows
 -- , ("M-f", sendMessage (T.Toggle "floats"))            -- Toggles all-float
 , ("M-S-t", sinkAll)                                  -- force tiling

--  Grid Select (CTRL-g followed by a key)
--  , ("M-g g", spawnSelected $ mygridConfig myApps)                 -- grid select favorite apps
 , ("M-g c", spawnSelected' myConfigs)              -- grid select useful config files
 , ("M-g g", gridselect (mygridConfig stringColorizer) myApps >>= maybe (pure ()) spawn)
--  , ("M-g g", spawnSelected $ spawnSelected defaultColorizer myApps) -- nope
 , ("M-g t", goToSelected   $ mygridConfig myColorizer)  -- goto selected window
 , ("M-g b", bringSelected  $ mygridConfig myColorizer) -- bring selected window

--  Windows navigation
--  , ("M-m", windows W.focusMaster)     -- Move focus to the master window
--  , ("M-j", windows W.focusDown)       -- Move focus to the next window
--  , ("M-k", windows W.focusUp)         -- Move focus to the prev window
--  , ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
--  , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
--  , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
--  , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
--  , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
--  , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
--  , ("M-S-s", windows copyToAll)
--  , ("M-C-s", killAllOtherCopies)

        -- Layouts
 , ("M-<Tab>", sendMessage NextLayout)                -- next layout
--  , ("M-C-M1-<Up>", sendMessage Arrange) -- ?
--  , ("M-C-M1-<Down>", sendMessage DeArrange) -- ?
 , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
 -- , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder

 , ("M-M1-u", sendMessage MirrorExpand)                 -- +h
--  , ("M-n", sendMessage Shrink)                       -- -w
--  , ("M-e", sendMessage MirrorShrink)                 -- -h
--  , ("M-i", sendMessage Expand)                       -- +w

--  Workspaces
 , ("M-<Right>", nextWS)  -- next ws
 , ("M-<Left>", prevWS)  -- prev ws
 , ("M-M1-<Right>", shiftToNext >> nextWS)       -- shift next ws
 , ("M-M1-<Left>", shiftToPrev >> prevWS)  -- shift prev ws
 , ("M1-<Space>", toggleWS' ["NSP"])

--  Controls for mocp music player.
 , ("M-u p", spawn "mocp --play")
 , ("M-u l", spawn "mocp --next")
 , ("M-u h", spawn "mocp --previous")
 , ("M-u <Space>", spawn "mocp --toggle-pause")

--  My Applications (Super+Alt+Key)
--  , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

--  Multimedia Keys
--  , ("<XF86AudioPlay>", spawn "cmus toggle")
--  , ("<XF86AudioPrev>", spawn "cmus prev")
--  , ("<XF86AudioNext>", spawn "cmus next")
 , ("<XF86AudioMute>", spawn "amixer set Master toggle")
 , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
 , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
--         , ("<Print>", spawn "scrotd 0")
 ]
 -- dynamic scratchpad keybinds
 ++ [("M-p " ++ k, namedScratchpadAction myScratchPads n) | (k,n) <- myScratchPads' ]
 ++ [("M-p M-" ++ k, namedScratchpadAction myScratchPads n) | (k,n) <- myScratchPads' ]
 -- dynamic search engine keybinds
 ++ [("M-s " ++ k, S.promptSearch promptConf' f) | (k,f) <- searchList ]
 ++ [("M-s M-" ++ k, S.promptSearch promptConf' f) | (k,f) <- searchList ]
 -- dynamic xprompt keybinds
 ++ [("M-/ " ++ k, f promptConf') | (k,f) <- promptList ]
 ++ [("M-/ M-" ++ k, f promptConf') | (k,f) <- promptList ]
--  ++ [("M-/ " ++ k, f promptConf' g) | (k,f,g) <- promptList' ]
 --  The following lines are needed for named scratchpads.
  where
   nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
   nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

---}
-- MAIN
{--}
main :: IO ()
main = do
 xmproc0 <- spawnPipe "xmobar" -- -x 0 /home/dt/.config/xmobar/xmobarrc0"
 xmonad $ ewmh def
  { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
  -- Run xmonad commands from command line with "xmonadctl command". Commands include:
  -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
  -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
  -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
  , handleEventHook =
   docksEventHook
   <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
  , modMask            = super
  , terminal           = myTerminal
  , startupHook        = myStartupHook
  , layoutHook         = showWName' myShowWNameTheme myLayoutHook
  , workspaces         = TS.toWorkspaces myWorkspaces
  , borderWidth        = borderW
  , normalBorderColor  = bg3
  , focusedBorderColor = fg1
  , logHook =
   workspaceHistoryHook
   <+> myLogHook
   <+> dynamicLogWithPP xmobarPP
    {-- ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
    , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
    , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
    , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
--     , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
    , ppHiddenNoWindows= \( _ ) -> ""       -- Only shows visible workspaces. Useful for TreeSelect.
    , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
    , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
    , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
    , ppExtras  = [windowCount]                           -- # of windows current workspace
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    --}
  } `additionalKeysP` myKeys
