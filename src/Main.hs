module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  scores <- loadHighScores
  playIO (InWindow "Shoot em Up" (800, 600) (100, 100)) -- Or FullScreen
        black            -- Background color
        60               -- Frames per second
        initialState { highScores = scores }
        view             -- View function
        input            -- Event function
        step             -- Step function
