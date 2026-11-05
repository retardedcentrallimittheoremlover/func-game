-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Maybe (mapMaybe)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Screen = Menu | Playing
              deriving (Eq, Show)

data Difficulty = Easy | Medium | Hard
                  deriving (Eq, Show)

-- screen dimensions and field margin
windowWidth :: Float
windowWidth = 800

windowHeight :: Float
windowHeight = 600

fieldMargin :: Float
fieldMargin = 10

fieldLeft, fieldRight, fieldBottom, fieldTop :: Float
fieldLeft = -(windowWidth / 2 - fieldMargin)
fieldRight = windowWidth / 2 - fieldMargin
fieldBottom = -(windowHeight / 2 - fieldMargin)
fieldTop = windowHeight / 2 - fieldMargin

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

data Explosion = Explosion { explosionPosition :: Position
                           , explosionLifetime :: Float
                           }

data GameState = GameState {
                   player       :: Player  
                 , keyUpDown    :: Bool
                 , keyDownDown  :: Bool 
                 , keySpaceDown :: Bool 
                 , bullets      :: [Bullet] 
                 , coolDown     :: Float 
                 , enemies      :: [Enemy] 
                 , explosions   :: [Explosion]
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
                  , explosionsCreated :: [Explosion]
                  , playerWasHit     :: Bool 
                  , enemiesDestroyed :: Int 
                  }

defaultPlayer :: Player
defaultPlayer = Player { playerPosition = Position (fieldLeft + 25) 0
                       , playerVelocity = Vector1 0 
                       }

bulletSpeed :: Float 
bulletSpeed = 500 

explosionDuration :: Float
explosionDuration = 0.15 -- seconds

kamiEnemy :: Enemy 
kamiEnemy = Enemy { enemyPosition = Position (fieldRight - 15) 0
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
               , explosions = []
               , spawnTimer = 0
               , spawnInterval = 0
               , score = 0
               , screen = Menu
               , isPaused = False
               , difficulty = Easy
               , highScores = [0, 0, 0]
               }

highscoreFile :: FilePath
highscoreFile = "highscores.txt"

loadHighScores :: IO [Int]
loadHighScores = do
  content <- catchIOError (readFile highscoreFile) (\_ -> return "")
  let scores = mapMaybe readMaybe (lines content)
  return (take 3 (scores ++ repeat 0))

saveHighScores :: [Int] -> IO ()
saveHighScores scores = writeFile highscoreFile (unlines (map show scores))
