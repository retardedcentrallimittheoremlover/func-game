-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Screen = Menu | Playing
              deriving (Eq, Show)

data Difficulty = Easy | Medium | Hard
                  deriving (Eq, Show)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Vector1 = Vector1 Float 
data Vector2 = Vector2 Float Float 

scaleV :: Float -> Vector2 -> Vector2 
scaleV s (Vector2 ax ay) = Vector2 (s*ax) (s*ay) 

magnitude :: Vector2 -> Float 
magnitude (Vector2 x y) = sqrt (x*x + y*y) 

normalize :: Vector2 -> Vector2 
normalize v@(Vector2 0 0) = Vector2 1 0 
normalize v@(Vector2 x y) = let 
                        m = magnitude v 
                    in scaleV (1/m) v 

data BoundingBox = BoundingBox Float Float  

class HasBoundingBox a where 
    boundingBox :: a -> BoundingBox 
    objectPosition :: a -> Position 

instance HasBoundingBox Enemy where 
    boundingBox e = enemySize e  
    objectPosition e = enemyPosition e 

instance HasBoundingBox Bullet where 
    boundingBox _ = BoundingBox 16 16 
    objectPosition b = bulletPosition b 

instance HasBoundingBox Player where 
    boundingBox _ = BoundingBox 30 30 
    objectPosition p = playerPosition p

data Position = Position { px :: Float, py :: Float} 

data Bullet = Bullet { bulletPosition :: Position
                     , bulletVelocity :: Vector2  
                     , bulletLifetime :: Float 
                     }

data Player = Player { playerPosition :: Position
                     , playerVelocity :: Vector1
                     }

data Enemy = Enemy { enemyPosition  :: Position 
                   , enemyVelocity :: Vector2
                   , enemySize      :: BoundingBox  
                   , enemySpeed     :: Float 
                   }

data GameState = GameState {
                   player       :: Player  
                 , keyUpDown    :: Bool
                 , keyDownDown  :: Bool 
                 , keySpaceDown :: Bool 
                 , bullets      :: [Bullet] 
                 , coolDown     :: Float 
                 , enemies      :: [Enemy] 
                 , spawnTimer   :: Float
                 , spawnInterval :: Float
                 , score        :: Int
                 , screen       :: Screen
                 , isPaused     :: Bool
                 , difficulty   :: Difficulty
                 , highScores   :: [Int]
                 }

data Collision = Collision {
                    remainingEnemies :: [Enemy] 
                  , remainingBullets :: [Bullet] 
                  , playerWasHit     :: Bool 
                  , enemiesDestroyed :: Int 
                  }

defaultPlayer :: Player 
defaultPlayer = Player { playerPosition = Position (-350) 0 
                       , playerVelocity = Vector1 0 
                       }

bulletSpeed :: Float 
bulletSpeed = 500 

kamiEnemy :: Enemy 
kamiEnemy = Enemy { enemyPosition = Position (350) 0 
                  , enemyVelocity = Vector2 (-1) 0
                  , enemySize      = BoundingBox 30 30  
                  , enemySpeed     = 300
                  } 


initialState :: GameState
initialState = GameState { 
                 player = defaultPlayer  
               , keyUpDown = False 
               , keyDownDown = False
               , keySpaceDown = False 
               , bullets = [] 
               , coolDown = 0
               , enemies = [kamiEnemy] 
               , spawnTimer = 0
               , spawnInterval = 0
               , score = 0
               , screen = Menu
               , isPaused = False
               , difficulty = Easy
               , highScores = [0, 0, 0]
               }
