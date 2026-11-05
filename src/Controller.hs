-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

step :: Float -> GameState -> IO GameState
step secs gstate
    | screen gstate /= Playing || isPaused gstate = return gstate
    | otherwise = return (moved { bullets = remainingBullets collision
                                , enemies = remainingEnemies collision
                                , score = score moved + enemiesDestroyed collision
                                })
    where
    p = player gstate
    p' = p { playerVelocity = Vector1 (playerdir gstate) }
    gstate' = bulletlist gstate { coolDown = max 0 (coolDown gstate - secs) }
    moved = spawnEnemies secs
          (gstate' { player = movePlayer secs p'
                   , bullets = keepBulletsInField (applyBullet secs (bullets gstate'))
                   , enemies = keepEnemiesInField (applyEnemies secs (enemies gstate'))
                   })
    collision = resolveCollisions (player moved) (bullets moved) (enemies moved)

movePlayer :: Float -> Player -> Player
movePlayer secs p =
    let Position x y = playerPosition p
        Vector1 vy = playerVelocity p
        speed = 250
        y' = max (-275) (min 275 (y + vy * secs * speed))
    in p { playerPosition = Position x y' }

input :: Event -> GameState -> IO GameState
input event gstate = return (inputKey event gstate)

playerdir :: GameState -> Float
playerdir gstate
    | keyUpDown gstate && not (keyDownDown gstate) = 1
    | keyDownDown gstate && not (keyUpDown gstate) = -1
    | otherwise = 0

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char '1') Down _ _) gs
    | screen gs == Menu = startGame Easy gs
inputKey (EventKey (Char '2') Down _ _) gs
    | screen gs == Menu = startGame Medium gs
inputKey (EventKey (Char '3') Down _ _) gs
    | screen gs == Menu = startGame Hard gs
inputKey (EventKey (Char 'p') Down _ _) gs
    | screen gs == Playing =
        gs { isPaused = not (isPaused gs)
           , keyUpDown = False
           , keyDownDown = False
           , keySpaceDown = False
           }
inputKey (EventKey (Char 'm') Down _ _) gs =
    updateHighScore gs { screen = Menu
                       , isPaused = False
                       , keyUpDown = False
                       , keyDownDown = False
                       , keySpaceDown = False
                       }
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs
    | screen gs == Playing && not (isPaused gs) = gs { keyUpDown = True }
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gs =
    gs { keyUpDown = False }
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs
    | screen gs == Playing && not (isPaused gs) = gs { keyDownDown = True }
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gs =
    gs { keyDownDown = False }
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs
    | screen gs == Playing && not (isPaused gs) = gs { keySpaceDown = True }
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gs =
    gs { keySpaceDown = False }
inputKey _ gstate = gstate

startGame :: Difficulty -> GameState -> GameState
startGame selectedDifficulty gs =
    gs { player = defaultPlayer
       , keyUpDown = False
       , keyDownDown = False
       , keySpaceDown = False
       , bullets = []
       , coolDown = 0
       , enemies = [kamiEnemy]
       , spawnTimer = 0
       , score = 0
       , screen = Playing
       , isPaused = False
       , difficulty = selectedDifficulty
       }

updateHighScore :: GameState -> GameState
updateHighScore gs =
    gs { highScores = replaceAt index (max (score gs) (highScores gs !! index))
                                  (highScores gs)
       }
    where index = difficultyIndex (difficulty gs)

difficultyIndex :: Difficulty -> Int
difficultyIndex Easy = 0
difficultyIndex Medium = 1
difficultyIndex Hard = 2

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index value values =
    take index values ++ [value] ++ drop (index + 1) values

spawnRate :: Difficulty -> Float
spawnRate Easy = 2.5
spawnRate Medium = 1.5
spawnRate Hard = 0.8

spawnEnemies :: Float -> GameState -> GameState
spawnEnemies secs gs =
    gs { enemies = enemies gs ++ newEnemies
       , spawnTimer = timer - fromIntegral numberToSpawn * rate
       }
    where
    rate = spawnRate (difficulty gs)
    timer = spawnTimer gs + secs
    numberToSpawn = floor (timer / rate) :: Int
    newEnemies = map (newEnemy (length (enemies gs))) [0 .. numberToSpawn - 1]

newEnemy :: Int -> Int -> Enemy
newEnemy existingCount index =
    kamiEnemy { enemyPosition = Position 350 y
              , enemySpeed = 300 + fromIntegral (index `mod` 3) * 25
              }
    where
    y = fromIntegral (((existingCount + index) * 83) `mod` 500) - 250

keepBulletsInField :: [Bullet] -> [Bullet]
keepBulletsInField = filter isInField
    where
    isInField bullet =
        let Position x y = bulletPosition bullet
        in x >= -382 && x <= 382 && y >= -282 && y <= 282

keepEnemiesInField :: [Enemy] -> [Enemy]
keepEnemiesInField = filter isInField
    where
    isInField enemy =
        let Position x y = enemyPosition enemy
        in x >= -375 && x <= 375 && y >= -275 && y <= 275

spawnBullet :: Player -> Float -> Bullet
spawnBullet p lifetime =
    let Position x y = playerPosition p
    in Bullet (Position x y) (Vector2 bulletSpeed 0) lifetime

moveBullet :: Float -> Bullet -> Bullet
moveBullet secs b =
    let Position x y = bulletPosition b
        Vector2 vx vy = bulletVelocity b
    in b { bulletPosition = Position (x + vx * secs) (y + vy * secs)
         , bulletLifetime = bulletLifetime b - secs
         }

bulletlist :: GameState -> GameState
bulletlist gs
    | keySpaceDown gs && coolDown gs <= 0 =
        gs { bullets = spawnBullet (player gs) 2 : bullets gs
           , coolDown = 0.2
           }
    | otherwise = gs

applyBullet :: Float -> [Bullet] -> [Bullet]
applyBullet secs = filter ((>= 0) . bulletLifetime) . map (moveBullet secs)

updateEnemy :: Float -> Position -> Enemy -> StdGen -> (Enemy, Maybe Bullet, StdGen)
updateEnemy = undefined

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy secs e =
    let Position x y = enemyPosition e
        Vector2 vx vy = enemyVelocitiy e
        speed = enemySpeed e
    in e { enemyPosition = Position (x + vx * secs * speed)
                              (y + vy * secs * speed)
         }

applyEnemies :: Float -> [Enemy] -> [Enemy]
applyEnemies secs = map (moveEnemy secs)

bulletHitsEnemy :: Bullet -> Enemy -> Bool
bulletHitsEnemy = collides

removeHitEnemies :: [Bullet] -> [Enemy] -> [Enemy]
removeHitEnemies bs = filter (\e -> not (any (`bulletHitsEnemy` e) bs))

removeHitBullets :: [Bullet] -> [Enemy] -> [Bullet]
removeHitBullets bs es = filter (\b -> not (any (`bulletHitsEnemy` b) es)) bs

removeEnemiesHitPlayer :: Player -> [Enemy] -> [Enemy]
removeEnemiesHitPlayer p = filter (\e -> not (enemyHitPlayer e p))

playerHit :: Player -> [Enemy] -> Bool
playerHit p = any (`collides` p)

enemyHitPlayer :: Enemy -> Player -> Bool
enemyHitPlayer = collides

resolveCollisions :: Player -> [Bullet] -> [Enemy] -> Collision
resolveCollisions p bs es =
    let rb = removeHitBullets bs es
        re = removeEnemiesHitPlayer p (removeHitEnemies bs es)
        ph = playerHit p es
        n = length es - length (removeHitEnemies bs es)
    in Collision { remainingEnemies = re
                 , remainingBullets = rb
                 , playerWasHit = ph
                 , enemiesDestroyed = n
                 }

directionTo :: Position -> Position -> Vector2
directionTo (Position px py) (Position ex ey) =
    normalize (Vector2 (px - ex) (py - ey))

spawnKami :: Position -> Position -> Enemy
spawnKami startPos targetPos =
    kamiEnemy { enemyPosition = startPos
              , enemyVelocitiy = directionTo targetPos startPos
              }

collides :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
collides a b =
    let (xa, xa', ya, ya') = getEdges a
        (xb, xb', yb, yb') = getEdges b
    in xa <= xb' && xa' >= xb && ya <= yb' && ya' >= yb

getEdges :: HasBoundingBox a => a -> (Float, Float, Float, Float)
getEdges a =
    let Position x y = objectPosition a
        BoundingBox w h = boundingBox a
    in (x - w / 2, x + w / 2, y - h / 2, y + h / 2)
