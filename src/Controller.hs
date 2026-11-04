-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return (gstate'' { player = movePlayer secs p'
                                    , bullets = applyBullet secs bs  
                                    , enemies = applyEnemies secs e 
                                    }) 
            where 
            p  = player gstate 
            p' = p { playerVelocity = Vector1 (playerdir gstate) } 
            bs = bullets $ bulletlist gstate' 
            gstate' = gstate { coolDown = max 0 (coolDown gstate - secs) }
            gstate'' = bulletlist gstate' 
            e = enemies gstate' 

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
bulletlist gs | keySpaceDown gs && (coolDown gs <= 0) = gs { bullets = b : bullets gs, coolDown = 0.2 }  
              | otherwise       = gs   
           where 
           b = spawnBullet (player gs) 2  

applyBullet :: Float -> [Bullet] -> [Bullet] 
applyBullet secs bs = filter (\b -> bulletLifetime b >= 0) $ map (moveBullet secs) bs 

--Enemies 

updateEnemy :: Float -> Position -> Enemy -> StdGen -> (Enemy, Maybe Bullet, StdGen) 
updateEnemy = undefined 

moveEnemy :: Float -> Enemy -> Enemy 
moveEnemy secs e = let 
                   Position x y  = enemyPosition e  
                   Vector2 vx vy = enemyVelocitiy e  
                   speed         = enemySpeed e  
                   y'            = y + vy * secs * speed 
                   x'            = x + vx * secs * speed 
                in e { enemyPosition = Position x' y' }

applyEnemies :: Float -> [Enemy] -> [Enemy] 
applyEnemies secs es = map (moveEnemy secs) es 


--BoundingBox 

collides :: (HasBoundingBox a, HasBoundingBox b) 
         => a -> b -> Bool 
collides a b = let 
               (xa, xa', ya, ya') = getEdges a 
               (xb, xb', yb, yb') = getEdges b 
        in xa <= xb' && xa' >= xb && 
           ya <= yb' && ya' >= yb 

getEdges :: HasBoundingBox a => a -> (Float, Float, Float, Float) 
getEdges a = let 
             Position x y = objectPosition a  
             BoundingBox w h = boundingBox a 
          in (x - w/2, x + w/2, y - h/2, y + h/2) 
