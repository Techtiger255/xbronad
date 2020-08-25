module Custom.Prompts where

import Custom.Vars

import XMonad
import Control.Arrow (first)
import Data.Char (isSpace)
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import qualified Data.Map as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

-- import XMonad.Util.EZConfig (additionalKeysP)
-- import XMonad.Prompt.Input  -- for custom prompts
-- import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)  -- for custom prompts


promptConf =
 def
  { font                = mfont
  , bgColor             = bg !! 0
  , fgColor             = fg !! 0
  , bgHLight            = bg !! 0
  , fgHLight            = fg !! 1
  , borderColor         = bg !! 2
  , promptBorderWidth   = 2
  , promptKeymap        = promptKeys
  , position            = Bottom -- CenteredAt { xpCenterY = 0.025, xpWidth = 1 } -- Top
  , height              = 18
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , autoComplete        = Just 250000 -- .25s  -- 10^5 -> .1s
  , showCompletionOnTab = True
  , searchPredicate     = fuzzyMatch -- isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Just 5 -- Nothing
--   , sorter              = fuzzyMatch
  }
promptConf' = promptConf { autoComplete = Nothing }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
mprompts =
 [ ("m", manPrompt)
 , ("/", runOrRaisePrompt)
 , ("S-/", shellPrompt)
 ]

--}
-- XPROMPT KEYMAP (emacs-like key bindings for xprompts)
{--}

promptKeys =
 M.fromList $
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
 ] -- `additionalKeysP` [("M-a", setSuccess True >> setDone True)]

--}
-- SEARCH ENGINES
{--}

news   = S.searchEngine "news"   "duckduckgo.com/?ia=news&iar=news&q="
urban  = S.searchEngine "slang"  "urbandictionary.com/define.php?term="
hoogle = S.searchEngine "hoogle" "hoogle.haskell.org/?hoogle="
arch   = S.searchEngine "arch"   "wiki.archlinux.org/index.php?search="

msearches
 =  [(x, S.duckduckgo) | x <- ["<Space>", "1"] ]
 ++ [(x, S.youtube)    | x <- ["y",       "2"] ]
 ++ [(x, arch)         | x <- ["a",       "3"] ]
 ++ [(x, hoogle)       | x <- ["h",       "4"] ]
 ++ [(x, news)         | x <- ["n",       "5"] ]
 ++ [(x, urban)        | x <- ["u",       "6"] ]


mprompts :: [(String, XPConfig -> X ())]
