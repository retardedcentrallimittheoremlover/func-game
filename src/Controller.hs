-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List 

step :: Float -> GameState -> IO GameState
step secs gstate
    | screen gstate /= Playing || isPaused gstate = return gstate
    | otherwise = do
        moved <- spawnEnemies secs updated
        shooting <- spawnEnemyBullets secs moved
        let (collected, remaining) = 
                collectPowerUps (player shooting) (powerUps shooting)                                                          
            afterCollect = shooting { powerUps = remaining } 
            powered = foldl (\gs pu -> activatePowerUp (powerUpType pu) gs) afterCollect collected 

            collision = resolveCollisions (player powered)
                                           (bullets powered)
                                           (enemyBullets powered)
                                           (enemies powered)
            nextState = powered  { bullets = remainingBullets collision
                                 , enemyBullets = remainingEnemyBullets collision
                                 , enemies = remainingEnemies collision
                                 , explosions = explosions shooting ++ explosionsCreated collision
                                 , score = score shooting + scoreGained collision
                                 }
        if playerWasHit collision && not (isShieldActive nextState)
           then do
               let gameOverState = updateHighScore nextState
                                     { screen = Menu
                                     , isPaused = False
                                     , keyUpDown = False
                                     , keyDownDown = False
                                     , keySpaceDown = False
                                     }
               saveHighScores (highScores gameOverState)
               return gameOverState
           else return nextState
    where
    p = player gstate
    p' = p { playerVelocity = Vector1 (playerdir gstate) }
    gstate' = bulletlist $ tickPowerUp secs $ gstate { coolDown = max 0 (coolDown gstate - secs) }
    updated = gstate' { player = movePlayer secs p'
                      , bullets = keepBulletsInField (applyBullet secs (bullets gstate'))
                      , enemyBullets = keepBulletsInField
                          (applyBullet secs (enemyBullets gstate'))
                      , enemies = keepEnemiesInField
                          (applyEnemies secs (py (playerPosition (player gstate'))) (enemies gstate'))
                      , explosions = updateExplosions secs (explosions gstate')
                      }

movePlayer :: Float -> Player -> Player
movePlayer secs p =
    let Position x y = playerPosition p
        Vector1 vy = playerVelocity p
        speed = 250
        y' = max (fieldBottom + 25) (min (fieldTop - 25) (y + vy * secs * speed))
    in p { playerPosition = Position x y' }
input :: Event -> GameState -> IO GameState
input event gstate = do
    let updatedState = inputKey event gstate
    case event of
        EventKey (Char 'm') Down _ _
            | screen gstate == Playing -> do
                saveHighScores (highScores updatedState)
                return updatedState
        _ -> return updatedState

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
       , enemyBullets = []
       , explosions = []
       , coolDown = 0
       , enemies = [kamiEnemy]
       , spawnTimer = 0
       , spawnInterval = 0
       , enemyShootTimer = 0
       , enemyShootInterval = 0
       , score = 0
       , screen = Playing
       , isPaused = False
       , difficulty = selectedDifficulty
       , activePowerUp = Nothing 
       , powerUpTimer = 0 
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

spawnEnemies :: Float -> GameState -> IO GameState
spawnEnemies secs gs = spawnDue (spawnTimer gs + secs) (spawnInterval gs) gs

spawnEnemyBullets :: Float -> GameState -> IO GameState
spawnEnemyBullets secs gs = do
    interval <- if enemyShootInterval gs > 0
                   then return (enemyShootInterval gs)
                   else randomShootInterval (difficulty gs)
    let timer = enemyShootTimer gs + secs
    if timer < interval
       then return gs { enemyShootTimer = timer
                      , enemyShootInterval = interval
                      }
       else do
           nextInterval <- randomShootInterval (difficulty gs)
           let newBullets = map enemyBullet (enemies gs)
           return gs { enemyBullets = enemyBullets gs ++ newBullets
                     , enemyShootTimer = timer - interval
                     , enemyShootInterval = nextInterval
                     }

randomShootInterval :: Difficulty -> IO Float
randomShootInterval difficulty =
    randomRIO (enemyShootMinInterval difficulty, enemyShootMaxInterval difficulty)

enemyBullet :: Enemy -> Bullet
enemyBullet enemy =
    let Position x y = enemyPosition enemy
    in Bullet (Position x y) (Vector2 (-enemyBulletSpeed) 0) 10

spawnDue :: Float -> Float -> GameState -> IO GameState
spawnDue timer interval gs = do
    nextInterval <- if interval > 0
                       then return interval
                       else randomSpawnInterval (difficulty gs)
    if timer < nextInterval
       then return (gs { spawnTimer = timer
                       , spawnInterval = nextInterval
                       })
       else do
           y <- randomSeparatedY (lastEnemyY (enemies gs))
           isTracker <- randomTracker
           let spawnedEnemy = if isTracker
                                then newTrackingEnemy (py (playerPosition (player gs)))
                                else newEnemy y
           next <- randomSpawnInterval (difficulty gs)
           spawnDue (timer - nextInterval) next
               (gs { enemies = enemies gs ++ [spawnedEnemy]
                   , spawnTimer = 0
                   , spawnInterval = 0
                   })

randomTracker :: IO Bool
randomTracker = do
       roll <- randomRIO (1 :: Int, 5)
       return (roll == 1)

randomSpawnInterval :: Difficulty -> IO Float
randomSpawnInterval Easy = randomRIO (2.0, 4.0)
randomSpawnInterval Medium = randomRIO (1.0, 2.5)
randomSpawnInterval Hard = randomRIO (0.4, 1.2)

randomSeparatedY :: Float -> IO Float
randomSeparatedY previousY = do
    y <- randomRIO (fieldBottom + 25, fieldTop - 25)
    if abs (y - previousY) >= 50
       then return y
       else randomSeparatedY previousY

lastEnemyY :: [Enemy] -> Float
lastEnemyY [] = 1000
lastEnemyY es =
    let Position _ y = enemyPosition (last es)
    in y

newEnemy :: Float -> Enemy
newEnemy y =
    kamiEnemy { enemyPosition = Position (fieldRight - 15) y
              , enemySpeed = enemySpeed kamiEnemy
              }

newTrackingEnemy :: Float -> Enemy
newTrackingEnemy playerY =
    kamiEnemy { enemyPosition = Position (fieldRight - 15) playerY
              , enemyType = TrackingEnemy
              }

keepBulletsInField :: [Bullet] -> [Bullet]
keepBulletsInField = filter isInField
    where
    isInField bullet =
        let Position x y = bulletPosition bullet
        in x >= fieldLeft + 8 && x <= fieldRight - 8
           && y >= fieldBottom + 8 && y <= fieldTop - 8
keepEnemiesInField :: [Enemy] -> [Enemy]
keepEnemiesInField = filter isInField
    where
    isInField enemy =
        let Position x y = enemyPosition enemy
        in x >= fieldLeft + 15 && x <= fieldRight - 15
           && y >= fieldBottom + 15 && y <= fieldTop - 15

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
           , coolDown = fireInterval gs 
           }
    | otherwise = gs

applyBullet :: Float -> [Bullet] -> [Bullet]
applyBullet secs = filter ((>= 0) . bulletLifetime) . map (moveBullet secs)

moveEnemy :: Float -> Float -> Enemy -> Enemy
moveEnemy secs playerY e =
    let Position x y = enemyPosition e
        Vector2 vx _ = enemyVelocity e
        speed = enemySpeed e
        verticalDirection
            | playerY > y + 5 = 1
            | playerY < y - 5 = -1
            | otherwise = 0
        verticalSpeed = if enemyType e == TrackingEnemy then 80 else 0
    in e { enemyPosition = Position (x + vx * secs * speed)
                              (y + verticalDirection * secs * verticalSpeed)
         }

applyEnemies :: Float -> Float -> [Enemy] -> [Enemy]
applyEnemies secs playerY = map (moveEnemy secs playerY)

updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs =
    filter ((> 0) . explosionLifetime)
    . map (\explosion -> explosion { explosionLifetime =
                                      explosionLifetime explosion - secs })

bulletHitsEnemy :: Bullet -> Enemy -> Bool
bulletHitsEnemy = collides

removeHitEnemies :: [Bullet] -> [Enemy] -> [Enemy]
removeHitEnemies bs = filter (\e -> not (any (`bulletHitsEnemy` e) bs))

removeHitBullets :: [Bullet] -> [Enemy] -> [Bullet]
removeHitBullets bs es = filter (\b -> not (any (bulletHitsEnemy b) es)) bs

removeEnemiesHitPlayer :: Player -> [Enemy] -> [Enemy]
removeEnemiesHitPlayer p = filter (\e -> not (enemyHitPlayer e p))

playerHit :: Player -> [Enemy] -> Bool
playerHit p = any (`collides` p)

enemyHitPlayer :: Enemy -> Player -> Bool
enemyHitPlayer = collides

resolveCollisions :: Player -> [Bullet] -> [Bullet] -> [Enemy] -> Collision
resolveCollisions p bs enemyBs es =
    let rb = removeHitBullets bs es
        bulletHitEnemies = filter (\e -> any (`bulletHitsEnemy` e) bs) es
        re = removeEnemiesHitPlayer p (removeHitEnemies bs es)
        remainingEnemyBs = filter (not . collides p) enemyBs
        ph = playerHit p es || any (collides p) enemyBs
        n = length es - length (removeHitEnemies bs es)
        points = sum (map enemyPoints bulletHitEnemies)
    in Collision { remainingEnemies = re
                 , remainingBullets = rb
                 , remainingEnemyBullets = remainingEnemyBs
                 , explosionsCreated = map explosionFor bulletHitEnemies
                 , playerWasHit = ph
                 , enemiesDestroyed = n
                 , scoreGained = points
                 }
    where
    explosionFor enemy = Explosion (enemyPosition enemy) explosionDuration
    enemyPoints enemy
        | enemyType enemy == TrackingEnemy = 2
        | otherwise = 1

directionTo :: Position -> Position -> Vector2
directionTo (Position px py) (Position ex ey) =
    normalize (Vector2 (px - ex) (py - ey))

spawnKami :: Position -> Position -> Enemy
spawnKami startPos targetPos =
    kamiEnemy { enemyPosition = startPos
              , enemyVelocity = directionTo targetPos startPos
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

collectPowerUps :: Player -> [PowerUp] -> ([PowerUp], [PowerUp]) 
collectPowerUps p ups = partition (collides p) ups 

activatePowerUp :: PowerUpType -> GameState -> GameState 
activatePowerUp Shield gs = gs { activePowerUp = Just Shield
                               , powerUpTimer = 5 
                               }   
activatePowerUp RapidFire gs = gs { activePowerUp = Just RapidFire
                                  , powerUpTimer = 3 
                                  } 

isShieldActive :: GameState -> Bool 
isShieldActive gs | activePowerUp gs == Just Shield && powerUpTimer gs > 0 = True 
                  | otherwise = False 

fireInterval :: GameState -> Float 
fireInterval gs | activePowerUp gs == Just RapidFire && powerUpTimer gs > 0 = 0.08 
                | otherwise = 0.2 
                
tickPowerUp :: Float -> GameState -> GameState 
tickPowerUp secs gs = 
    let t = max 0 (powerUpTimer gs - secs) 
    in gs { powerUpTimer = t 
          , activePowerUp = 
                if t == 0 then Nothing else activePowerUp gs 
          } 


