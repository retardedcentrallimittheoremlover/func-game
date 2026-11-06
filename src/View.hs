-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate
    | screen gstate == Menu = pictures [fieldBorder, menuView gstate]
    | otherwise = pictures [fieldBorder, pictures (gameView gstate ++ pauseOverlay gstate)]

fieldBorder :: Picture
fieldBorder = color (greyN 0.6)
            (rectangleWire (windowWidth - 2 * fieldMargin)
                           (windowHeight - 2 * fieldMargin))

gameView :: GameState -> [Picture]
gameView gstate = [drawPlayer (player gstate), scoreView (score gstate)]
                ++ map drawBullet (bullets gstate)
                ++ map drawEnemyBullet (enemyBullets gstate)
                ++ map drawEnemy (enemies gstate)
                ++ map drawExplosion (explosions gstate)
                ++ map drawPowerUp (powerUps gstate) 

menuView :: GameState -> Picture
menuView gstate = pictures
    [ menuText 34 190 "SHOOT EM UP"
    , menuText 18 105 "Choose a difficulty"
    , menuText 20 40    ("1  Easy       Highscore: " ++ show (highScoreAt 0 gstate))
    , menuText 20 (-5)  ("2  Medium     Highscore: " ++ show (highScoreAt 1 gstate))
    , menuText 20 (-50) ("3  Hard       Highscore: " ++ show (highScoreAt 2 gstate))
    ]

pauseOverlay :: GameState -> [Picture]
pauseOverlay gstate
    | isPaused gstate = [textAt (-90) 0 36 "PAUSED"]
    | otherwise = []

scoreView :: Int -> Picture
scoreView currentScore = textAt (fieldLeft + 10) (fieldTop - 30) 20 ("Score: " ++ show currentScore)

textAt :: Float -> Float -> Float -> String -> Picture
textAt x y size label = translate x y (scale factor factor (color white (text label)))
    where factor = size / 100

menuText :: Float -> Float -> String -> Picture
menuText size y label = textAt (fieldLeft + 10) y size label

highScoreAt :: Int -> GameState -> Int
highScoreAt index gstate = highScores gstate !! index

drawPlayer :: Player -> Picture 
drawPlayer p = let 
               Position x y = playerPosition p 
               in translate x y $ color cyan $ polygon [(-20,-15), (20,0), (-20,15)]

drawBullet :: Bullet -> Picture 
drawBullet b = let 
               Position x y = bulletPosition b 
               in translate x y $ color yellow $ circleSolid 8 

drawEnemyBullet :: Bullet -> Picture
drawEnemyBullet b =
    let Position x y = bulletPosition b
    in translate x y $ color orange $ circleSolid 8

drawEnemy :: Enemy -> Picture 
drawEnemy e = let 
              Position x y = enemyPosition e 
              enemyColor = if enemyType e == TrackingEnemy then orange else red
              in translate x y $ color enemyColor $ rectangleSolid 30 30

drawExplosion :: Explosion -> Picture
drawExplosion explosion =
    let Position x y = explosionPosition explosion
        progress = 1 - explosionLifetime explosion / explosionDuration
        radius = 10 + 15 * progress
    in translate x y $ pictures
        [ color orange (circleSolid radius)
        , color yellow (circleSolid (radius * 0.66))
        ]

drawPowerUp :: PowerUp -> Picture 
drawPowerUp pw = let 
                 Position x y = powerUpPosition pw 
                 pwColor = if powerUpType pw == Shield then blue else green 
                 in translate x y $ color pwColor $ circleSolid 11 



