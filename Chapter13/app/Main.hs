{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Gloss

type R = Double

-------------------
-- * Exercise 13.1
-------------------
-- Make an interesting picture using the display function.
--

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

redDisk :: Picture
redDisk = Color red (disk 100)

wholePicture :: Picture
wholePicture = Pictures [Translate 0 0 whiteFlag
                        ,Translate 0 0 redDisk
                        ]


main :: IO ()
main = display displayMode black wholePicture
