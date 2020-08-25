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

--}
-- hooks
{--}
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, pad, xmobarPP, xmobarColor, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers -- (isFullscreen, doFullFloat, isDialog)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

--}
-- layouts
{--}

-- variants
import qualified XMonad.Layout.Spiral as LS
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion

-- effects
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Actions.CopyWindow

-- modifiers
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows (boringWindows, boringAuto)
import XMonad.Actions.MouseResize
import XMonad.Actions.DynamicProjects

--}
-- utils
{--}
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP, checkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- import XMonad.Hooks.ManageHelpers

--}
-- urgencyhook
{--}
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run


import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation


import qualified XMonad.StackSet as W
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
 urgencyHook LibNotifyUrgencyHook w = do
  name     <- getName w
  Just ws <- fmap (W.findTag w) $ gets windowset
  safeSpawn mnotify' $ mnotify'' ++ [ws, show name]

--}
-- AUTOSTART
{--}

mstartupHook = do
 -- note: (spawnpipe mbar) is used in main()
 mapM_ (\x -> spawnOnce $ x ++ " &") $ mlock ++ msession
 mapM_ (\x -> unspawn x >> spawn ("sleep 1;" ++ x)) $ mtray : mrestart
 where
  mlock =
   [ "xset s " ++ show (60 * mtimeout)
   , "xss-lock " ++ mlocker
   ]

--}
-- WORKSPACES
{--}
tagWS tag l = [(a ++ tag, b) | (a, b) <- l ]
chop l = [a | (a, b) <- l]
mWS name directory startup = ( name, (directory, startup) )

mregular' =
 tagWS mreg
 [ mWS "work" "~/"
   $ mapM_ spawn [mterminal', mfiles]
 , mWS "dev" "~/"
   $ mapM_ spawn [mterminal, meditor ""]
 , mWS "web" "~/"
   $ mapM_ spawn [mbrowser]
 , mWS "art" "~/"
   $ mapM_ spawn ["gravit-designer", "gimp"]
 , mWS "chat" "~/"
   $ mapM_ spawn ["discord", "thunderbird"]
 ]
mfloats' =
 tagWS mfloat
 [ mWS "A1" "~/"
   $ mapM_ spawn []
 , mWS "A2" "~/"
   $ mapM_ spawn []
 , mWS "A3" "~/"
   $ mapM_ spawn []
 ]
mfulls' =
 tagWS mfull
 [ mWS "B1" "~/"
   $ mapM_ spawn ["lutris"]
 , mWS "B2" "~/"
   $ mapM_ spawn []
 , mWS "B3" "~/"
   $ mapM_ spawn []
 ]

mregular = chop mregular'
mfloats = chop mfloats'
mfulls = chop mfulls'

mworkspaces =
 concat
 [ mregular, mfloats, mfulls
 , ["NSP"]
 ]

mprojects =
 [
 Project
  { projectName = a
  , projectDirectory = b 
  , projectStartHook = Just $ do c
  } | (a, (b, c)) <- concat [ mregular', mfloats', mfulls' ]
 ]

--}
-- HOOKS
{--}

-- tail "abc" -> "bc"
--  "Abc123"  ~? "abc" == True   -- tailed infix
--  "ABc"     ~? "Abc" == False
--  "123abc" ~~? "abc" == True   -- infix
--  "abc"    ~~? "Abc" == False
--  "abc"     =? "abc" == True   -- exact
--  "abc1"    =? "abc" == False

--  | operator | false-positivity | verbosity |
--  -------------------------------------------
--  | ~?       | high             | low       |
--  | ~~?      | medium           | medium    |
--  | =?       | low              | high      |

-- usage:
-- act :: X () -> Functor -> [managehook-thing] -> [[Char]] -> [[Char]] -> ManageHook
-- act action joining_operator [identifier] [tailed] [exact], eg:
--  act (shift 1) (>>|) [className] ["abc"] ["123"] = (never, className ~? "abc" <||> className ~~? "123") --> shift 1
--  act (shift 1) (>>&) [className, title] ["urmom"] [] = (never, className ~? "urmom" <&&> title ~? "title") --> shift 1
--  act (shift 1) (>>|) [className, title] [] [] = (never) --> shift 1
mmanageHook =
 composeOne
  [ transience
  , doCenterFloat >?| [ isDialog, currentWs ~~? mfloat ]
  , act doCenterFloat (>?|) [className, title]
    [ "pop-up"
    , "overlay"
    , "toolkit"
    , "picture"
    , "dialog"
    , "message"
    , "notif"
    , "zenity"
    , "spotify"
    ] ["man"]
  , doFullFloat >?| [ isFullscreen, currentWs ~~? mfull ]
  , act doFullFloat (>?|) [className, title] ["steam"] []
  ]
 <+> composeAll
   [ act (s 1) (>>|) [className] ["obs"] []
   , act (s 2) (>>|) [className] ["jetbrains"] []
   , act (s 3) (>>|) [className] ["firefox", "brave"] []
   , act (s 4) (>>|) [className] ["krita", "gimp", "design"] []
   , act (s 5) (>>|) [className] ["discord", "thunderbird", "messeng" , "convers"] ["irc", "chat"]
   
   , act (s' 1) (>>|) [className] ["tor-browser"] []
   , act (s' 2) (>>|) [className, title] ["music", "video"] ["mpv", "vlc"]
   , act (s' 3) (>>|) [className] ["xephyr", "virt"] []
 
   , act (s'' 1) (>>|) [className] ["steam", "lutris"] []
   ]
 <+> namedScratchpadManageHook mscratch
 <+> manageDocks
 where
  never = ( stringProperty "_" =? "_" )
  s n = doShift $ mregular!n
  s' n = doShift $ mfloats!n
  s'' n = doShift $ mfulls!n
  act fn oper values n0 n1 =
   fn `oper`
   concat
   [  [never]
   ++ [x ~? y  | y <- n0]
   ++ [x ~~? y | y <- n1]
      | x <- values ]
  or' a = foldr1 (<||>) a
  and' a = foldr1 (<&&>) a
  (>>|) a b = or' b --> a
  (>>&) a b = and' b --> a
  (>?|) a b = or' b -?> a
  (>?&) a b = and' b -?> a
 
mhandleEventHook =
 docksEventHook
 <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

mlogHook pipe =
 fadeInactiveLogHook 0.8
 <+> workspaceHistoryHook
 <+> mbarHook pipe

mbarHook pipe = do
 copies <- wsContainingCopies
 let check x | x `elem` copies = fc (fg!3) (bg!2) $ " + "
                   | otherwise = fc (fg!1) (bg!3) $ pad x
 dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
   { ppOutput = \x -> hPutStrLn pipe x

   , ppSep             =  " "
   , ppUrgent          = fc(fg!3) (bg!3) . pad
   , ppCurrent         = fc(bg!1) (fg!2) . pad
   , ppHidden          = check
   , ppHiddenNoWindows = fc(bg!4) (bg!2) . pad . \x -> take 2 x
   , ppTitle           = fc(fg!5) (bg!2) . pad
   
   , ppExtras = [windowCount]
   , ppOrder  = \(ws:l:t:ex) -> [l, ws] ++ ex ++ [t]
   }
  where fc = xmobarColor


mlayoutHook mBG mFG =
   mods
   -- windowNavigation .
   -- subLayout [0] (tabs mBG mFG)
 $ accordion
   ||| spiral
   ||| tabs mBG mFG
 where
  mods =
     smartBorders .
     -- withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = mFG } urgencyConfig { suppressWhen = Never } .
     showWName' (mshowWNameTheme mBG mFG) .
     avoidStruts .
     boringAuto .
     -- boringWindows .
     mouseResize

--}
-- LAYOUTS
{--}

mspacing i = spacingRaw True (Border (i*2) (i*2) (i*4) (i*4)) True (Border i i i i) True
nospace = spacingRaw False bnull False bnull False where bnull = Border 0 0 0 0
lay name count space layout =
   -- windowArrange $
   renamed [Replace name]
 $ (if space > 0 then mspacing space else nospace)
 $ limitWindows count
 $ minimize
 $ layout

accordion =    lay "acc" 6  3 $ Accordion
spiral =       lay "spi" 4  4 $ LS.spiral (3/5)
tabs mBG mFG = lay "tab" 50 0
 $ tabbed shrinkText mtabConfig
 where
  mtabConfig =
   def
   { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=16"
   , activeColor         = mFG
   , activeBorderColor   = mBG
   , activeTextColor     = mBG
   
   , inactiveColor       = mBG
   , inactiveBorderColor = mBG
   , inactiveTextColor   = mFG
   }

mshowWNameTheme mBG mFG =
 def
 { swn_font    = "xft:Mononoki Nerd Font:regular:pixelsize=32"
 , swn_fade    = 0.5
 , swn_bgcolor = mBG
 , swn_color   = mFG
 }

---}
-- MAIN
{--}
main = do
 pipe <- spawnPipe mbar
 fgc <- rand fg'
 bgc <- rand bg'
 let mBG = bgc {-bg!2-}; mFG = fgc {-fg!1-} in
  xmonad
  $ withUrgencyHook LibNotifyUrgencyHook
  $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = mFG } urgencyConfig { suppressWhen = Never }
  $ dynamicProjects mprojects
  $ ewmh def
   { manageHook = mmanageHook
   , handleEventHook = mhandleEventHook
   , logHook = mlogHook pipe
 
   , modMask     = super
   , terminal    = mterminal
   , startupHook = mstartupHook
   , layoutHook  = mlayoutHook mBG mFG
   
   , workspaces         = mworkspaces
   , borderWidth        = borderW
   , normalBorderColor  = mBG
   , focusedBorderColor = mFG
   
   , focusFollowsMouse  = False
   , clickJustFocuses   = True
   }
   `additionalKeysP` moverrides
   `removeKeysP` [ a | (a,b) <- keys]
   `additionalKeysP` keys
  where
   keys =
    ("M-h", checkKeymap (ewmh def `additionalKeysP` mkeys) mkeys) : mkeys


main :: IO ()
mstartupHook :: X ()
mmanageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
mshowWNameTheme :: [Char] -> [Char] -> SWNConfig
