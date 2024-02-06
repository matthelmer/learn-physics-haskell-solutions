{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Gloss

type R = Double

-------------------
-- * Exercise 13.1
-------------------
-- Make an interesting picture using the display function.

displayMode :: Display
displayMode = InWindow "My Window" (1000, 700) (10, 10)

blueCircle :: Picture
blueCircle = Color blue (Circle 100)

flagRectangle :: Picture
flagRectangle = rectangleSolid 500 300

whiteFlag :: Picture
whiteFlag = Color white flagRectangle

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

risingSunDisk :: Picture
risingSunDisk = Color red (disk 100)

wholePicture :: Picture
wholePicture = Pictures [Translate 0 0 whiteFlag
                        ,Translate 0 0 risingSunDisk
                        ]


-------------------
-- * Exercise 13.2
-------------------
-- Use animate to make a simple animation.
redDisk :: Picture
redDisk = Color red (disk 25)

projectileMotion :: Float -> Picture
projectileMotion t = Translate (xDisk t) (yDisk t) wholePicture

xDisk :: Float -> Float
xDisk t = 40 * t

yDisk :: Float -> Float
yDisk t = 80 * t - 4.9 * t**2


-------------------
-- * Exercise 13.3
-------------------
-- Use animate to make the red disk oscillate left and right. Then change your code to make the red disk orbit in a circle. Can you make the red disk move in an ellipse?

main :: IO ()
main = animate displayMode black projectileMotion
