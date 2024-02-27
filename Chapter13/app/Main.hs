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
redDisk = Color red (disk 10)

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
circularMotion t = Translate (500 * cos t) (500 * sin t) redDisk

ellipticalMotion :: Float -> Picture
ellipticalMotion t = Translate (300 * cos t) (100 * sin t) redDisk

--main :: IO ()
--main = animate displayMode black ellipticalMotion

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
--rate :: Int
--rate = 35

-- model
--type State = (Float,Float)

--initialState :: State
--initialState = (0,0)

-- function that describes what pic to produce given a value of type model
--displayFunc :: State -> Picture
--displayFunc (x,y) = Translate x y redDisk

--updateFunc :: Float -> State -> State
--updateFunc dt (x,y) = (100 * sin(x**2 - y**2 + 5.655),
--                      100 * cos(2 * x * y + 5.16))

--main :: IO ()
--main = simulate displayMode black rate initialState displayFunc (\_ -> updateFunc)

-------------------
-- * Exercise 13.6
-------------------
-- Modify Listing 13-2 code so one meter is represented by 10 pixels, instead of 1 pixel. Change initial velocity so you can see it.
rate :: Int
rate = 24
type Position = (Float,Float)
type Velocity = (Float,Float)
type State = (Position,Velocity)

initialState :: State
initialState = ((0,0),(4,8)) -- position and velocity, each x,y values

displayFunc :: State -> Picture
displayFunc ((x,y),_) = Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt ((x,y),(vx,vy))
    = (( x + 10 * vx * dt, y + 10 * vy * dt)
      ,(vx          ,vy - 9.8 * dt))

main :: IO ()
main = simulate displayMode black rate initialState displayFunc
       (\_ -> updateFunc)
