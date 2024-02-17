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
oscillatingMotion :: Float -> Picture
oscillatingMotion t = Translate (100 * sin t) (0) redDisk

circularMotion :: Float -> Picture
circularMotion t = Translate (100 * cos t) (100 * sin t) redDisk

ellipticalMotion :: Float -> Picture
ellipticalMotion t = Translate (300 * cos t) (100 * sin t) redDisk


-------------------
-- * Exercise 13.4
-------------------
-- Use animate to achieve the same motion of red disk we achieved with simulate in listing 13-1.
redDiskAnimatedSimulation :: Float -> Picture
redDiskAnimatedSimulation t = Translate (10*t) ((-5)*t) redDisk
--main :: IO ()
--main = animate displayMode black redDiskAnimatedSimulation

-------------------
-- * Exercise 13.5
-------------------
-- Use simulate to do something interesting/creative.

-- updates per second of real time
rate :: Int
rate = 1

-- model
type State = (Float,Float)

initialState :: State
initialState = (100,0)

-- function that describes what pic to produce given a value of type model
displayFunc :: State -> Picture
displayFunc (x,y) = Translate x y wholePicture

updateFunc :: Float -> State -> State
updateFunc dt (x,y) = (10 * cos(x + dt), 10 * sin(y + dt))


main :: IO ()
main = simulate displayMode black rate initialState displayFunc (\_ -> updateFunc)
