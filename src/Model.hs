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

data Position = Position { px :: Float, py :: Float} 

data Bullet = Bullet { bulletPosition :: Position
                     , bulletVelocity :: Vector2  
                     , bulletLifetime :: Float 
                     }

data Player = Player { playerPosition :: Position
                     , playerVelocity :: Vector1
                     }

data GameState = GameState {
                   player       :: Player  
                 , keyUpDown    :: Bool
                 , keyDownDown  :: Bool 
                 , keySpaceDown :: Bool 
                 , bullets      :: [Bullet] 
                 }

defaultPlayer :: Player 
defaultPlayer = Player { playerPosition = Position (-350) 0 
                       , playerVelocity = Vector1 0 
                       }

bulletSpeed :: Float 
bulletSpeed = 500 

initialState :: GameState
initialState = GameState { 
                 player = defaultPlayer  
               , keyUpDown = False 
               , keyDownDown = False
               , keySpaceDown = False 
               , bullets = [] 
               }
