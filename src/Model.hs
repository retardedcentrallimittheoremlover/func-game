-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Vector1 = Vector1 Float 
data Vector2 = Vector2 Float Float 

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
                   , enemyVelocitiy :: Vector2 
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
                 }

defaultPlayer :: Player 
defaultPlayer = Player { playerPosition = Position (-350) 0 
                       , playerVelocity = Vector1 0 
                       }

bulletSpeed :: Float 
bulletSpeed = 500 

kamiEnemy :: Enemy 
kamiEnemy = Enemy { enemyPosition = Position (350) 0 
                  , enemyVelocitiy = Vector2 (-1) 0 
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
               }
