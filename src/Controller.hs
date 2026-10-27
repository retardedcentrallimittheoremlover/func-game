-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return (movePlayer secs gstate) 

-- | Move player function 
movePlayer :: Float -> GameState -> GameState 
movePlayer secs gstate = 
    gstate { playerPosition = Position x newY } 
    where 
      Position x y = playerPosition gstate 

      speed = 250 

      direction 
        | keyUpDown gstate && not (keyDownDown gstate) = 1 
        | keyDownDown gstate && not (keyUpDown gstate) = -1 
        | otherwise = 0 

      newY = max (-280) (min 280 (y + direction * speed * secs)) 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)



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

inputKey _ gstate = gstate -- Otherwise keep the same

