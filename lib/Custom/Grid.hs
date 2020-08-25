module Custom.Grid where

import Custom.Vars

import XMonad
import XMonad.Actions.GridSelect
import Data.Bool
import Data.String
import System.Random (randomRIO, randomR)
import Data.Random.List (randomElement)


gsconfig =
 (buildDefaultGSConfig colors)
  { gs_cellheight   = 50
  , gs_cellwidth    = 75
  , gs_cellpadding  = 7
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = mfont
  }


mgrids = 
 [ ("<Backspace>", bringSelected gsconfig)  -- bring selected window
 , ("]", custom spawn mconfigs)             -- my configs
 , ("\\", custom spawn mapps)               -- my apps
 , ("<Return>", goToSelected gsconfig)      -- goto selected window
 ]


custom fn items = gridselect gsconfig items >>= maybe (pure ()) fn
rand l = (l!) <$> randomRIO (1, length l)
colors _ active = do
 fg <- rand fg'
 bg <- rand bg'
 if active
  then return (fg, bg)
  else return (bg, fg)


gsconfig :: GSConfig a
colors :: a -> Bool -> X (String, String)
custom :: (String -> X ()) -> [(String, String)] -> X ()
