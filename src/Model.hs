-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Position = Position { px :: Float, py :: Float} 


data GameState = GameState {
                 playerPosition :: Position 
                 , keyUpDown    :: Bool
                 , keyDownDown  :: Bool 
                 }

initialState :: GameState
initialState = GameState { 
               playerPosition = Position (-300) 0 
               , keyUpDown = False 
               , keyDownDown = False
               }
