-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures (p : bs) 
                where 
                p = drawPlayer $ player gstate  
                bs = map drawBullet $ bullets gstate  

drawPlayer :: Player -> Picture 
drawPlayer p = let 
               Position x y = playerPosition p 
               in translate x y $ color cyan $ polygon [(-20,-15), (20,0), (-20,15)]

drawBullet :: Bullet -> Picture 
drawBullet b = let 
               Position x y = bulletPosition b 
               in translate x y $ color yellow $ circleSolid 8 
