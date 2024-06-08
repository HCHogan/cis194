module Hw7 (hw7Main) where

import Hw7.Editor
import Hw7.StringBuffer

hw7Main :: IO ()
hw7Main =
         runEditor editor $
                  unlines
                           [ "This buffer is for notes you don't want to save, and for"
                           , "evaluation of steam valve coefficients."
                           , "To load a different file, type the character L followed"
                           , "by the name of the file."
                           ]
