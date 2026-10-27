-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = translate x y $ color cyan $
                  polygon [(-20,-15), (20,0), (-20,15)]
                  where 
                  Position x y = playerPosition gstate 
