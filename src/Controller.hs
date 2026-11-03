-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return (gstate { player = movePlayer secs p' }) 
            where 
            p  = player gstate 
            p' = p { playerVelocity = Vector1 (playerdir gstate) } 

-- | Move player function 
movePlayer :: Float -> Player -> Player  
movePlayer secs p     =  
    let  Position x y = playerPosition p  
         Vector1 vy   = playerVelocity p 
         speed        = 250 
         y'           = y + vy * secs * speed  
    in p { playerPosition = Position x y' } 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

playerdir :: GameState -> Float  
playerdir gstate | keyUpDown gstate && not (keyDownDown gstate) = 1
                 | keyDownDown gstate && not (keyUpDown gstate) = -1 
                 | otherwise = 0 

inputKey :: Event -> GameState -> GameState
--Up arrow key pressed
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs = 
    gs { keyUpDown = True} 

--Up arrow key released 
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gs = 
    gs { keyUpDown = False} 

--Down arrow key pressed 
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs = 
    gs { keyDownDown = True } 

--Down arrow released 
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gs = 
    gs { keyDownDown = False } 

--Spacebar 
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs = 
    gs { keySpaceDown = True } 

inputKey (EventKey (SpecialKey KeySpace) Up _ _) gs = 
    gs { keySpaceDown = False } 

inputKey _ gstate = gstate -- Otherwise keep the same

--Bullets 
spawnBullet :: Player -> Float -> Bullet 
spawnBullet p f =
            let Position x y    = playerPosition p 
                bv              = Vector2 bulletSpeed 0     
            in Bullet (Position x y) bv f  

moveBullet :: Float -> Bullet -> Bullet 
moveBullet secs b = 
            let Position x y    = bulletPosition b 
                Vector2 vx0 vy0 = bulletVelocity b 
                bpos            = Position (x + vx0 * secs) (y + vy0 * secs)
                blife           = bulletLifetime b - secs 
            in b { bulletPosition = bpos, bulletLifetime = blife } 

bulletlist :: GameState -> GameState  
bulletlist gs | keySpaceDown gs = gs { bullets = b : bullets gs }  
              | otherwise       = gs { bullets = bullets gs }  
           where 
           b = spawnBullet (player gs) 0  

