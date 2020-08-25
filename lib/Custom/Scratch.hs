module Custom.Scratch where

import Custom.Vars

import XMonad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W



mscratch' =
 [ "t", "n", "m", "f", "p" ]

mscratch =
 [ NS (s!1) t tF dC
 , NS (s!2) n nF dC
 , NS (s!3) m mF dC
 , NS (s!4) f fF dC
 , NS (s!5) p pF dC
 ]
 where
  s = mscratch'
  sn = [ "S:" ++ (caps x) | x <- s ]
  dC = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 -h
    l = 0.95 -w

  t  = mconsole (sn!1) mshell
  tF = resource =? (sn!1) <||> title =? (sn!1)

  n  = mconsole (sn!2) meditor'  -- assumes CLI
  nF = resource =? (sn!2) <||> title =? (sn!2)

  m  = "spotify"
  mF = resource ~? "spotify"

  f  = mfiles
  fF = className ~? mfiles

  p  = "bitwarden"
  pF = className ~? "bitwarden"
