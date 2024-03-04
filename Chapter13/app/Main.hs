{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Graphics.Gloss as Gloss
import Vis
import Linear
import SpatialMath

type R = Double

-------------------
-- * Exercise 13.1
-------------------
-- Make an interesting picture using the display function.

displayMode :: Gloss.Display
displayMode = Gloss.InWindow "My Window" (1000, 700) (10, 10)

blueCircle :: Gloss.Picture
blueCircle = Gloss.Color Gloss.blue (Gloss.Circle 100)

flagRectangle :: Gloss.Picture
flagRectangle = Gloss.rectangleSolid 500 300

whiteFlag :: Gloss.Picture
whiteFlag = Gloss.Color Gloss.white flagRectangle

disk :: Float -> Gloss.Picture
disk radius = Gloss.ThickCircle (radius / 2) radius

risingSunDisk :: Gloss.Picture
risingSunDisk = Gloss.Color Gloss.red (disk 100)

wholePicture :: Gloss.Picture
wholePicture = Gloss.Pictures [Gloss.Translate 0 0 whiteFlag
                              ,Gloss.Translate 0 0 risingSunDisk
                              ]

-------------------
-- * Exercise 13.2
-------------------
-- Use animate to make a simple animation.
redDisk :: Gloss.Picture
redDisk = Gloss.Color Gloss.red (disk 10)

projectileMotion :: Float -> Gloss.Picture
projectileMotion t = Gloss.Translate (xDisk t) (yDisk t) wholePicture

xDisk :: Float -> Float
xDisk t = 40 * t

yDisk :: Float -> Float
yDisk t = 80 * t - 4.9 * t**2

-------------------
-- * Exercise 13.3
-------------------
-- Use animate to make the red disk oscillate left and right. Then change your code to make the red disk orbit in a circle. Can you make the red disk move in an ellipse?
oscillatingMotion :: Float -> Gloss.Picture
oscillatingMotion t = Gloss.Translate (100 * sin t) 0 redDisk

circularMotion :: Float -> Gloss.Picture
circularMotion t = Gloss.Translate (500 * cos t) (500 * sin t) redDisk

ellipticalMotion :: Float -> Gloss.Picture
ellipticalMotion t = Gloss.Translate (300 * cos t) (100 * sin t) redDisk

-- main :: IO ()
-- main = Gloss.animate displayMode Gloss.black ellipticalMotion

-------------------
-- * Exercise 13.4
-------------------
-- Use animate to achieve the same motion of red disk we achieved with simulate in listing 13-1.
redDiskAnimatedSimulation :: Float -> Gloss.Picture
redDiskAnimatedSimulation t = Gloss.Translate (10*t) ((-5)*t) redDisk
-- main :: IO ()
-- main = Gloss.animate displayMode Gloss.black redDiskAnimatedSimulation

-------------------
-- * Exercise 13.5
-------------------
-- Use simulate to do something interesting/creative.

-- updates per second of real time
-- rate :: Int
-- rate = 35

-- model
-- type State = (Float,Float)

-- initial state
-- initialState :: State
-- initialState = (0,0)

-- function that describes what pic to produce given a value of type model
-- displayFunc :: State -> Gloss.Picture
-- displayFunc (x,y) = Gloss.Translate x y redDisk

-- updateFunc :: Float -> State -> State
-- updateFunc dt (x,y) = (100 * sin(x**2 - y**2 + 5.655),
--                       100 * cos(2 * x * y + 5.16))

-- main :: IO ()
-- main = Gloss.simulate displayMode Gloss.black rate initialState displayFunc (\_ -> updateFunc)

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

displayFunc :: State -> Gloss.Picture
displayFunc ((x,y),_) = Gloss.Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt ((x,y),(vx,vy))
    = (( x + 10 * vx * dt, y + 10 * vy * dt)
      ,(vx          ,vy - 9.8 * dt))

-- main :: IO ()
-- main = Gloss.simulate displayMode Gloss.black rate initialState displayFunc
--        (\_ -> updateFunc)

-------------------
-- * Exercise 13.7
-------------------
-- Try to use simulate to make the red disk oscillate left and right without explicitly giving it an oscillating function like sin or cos.
oscillatingLikeMotion :: Float -> Gloss.Picture
oscillatingLikeMotion t = Gloss.Translate (100 * sin t) 0 redDisk

-------------------
-- * Exercise 13.8
-------------------
-- Rewrite the 3D axes code so that the x-axis points to the right, the y-axis points upward, and the z-axis points out of the page. This is my second favorite coordinate system.

axes :: VisObject R
axes = VisObjects [Line Nothing [V3 0 0 0, V3 1 0 0] red
                  ,Line Nothing [V3 0 0 0, V3 0 1 0] green
                  ,Line Nothing [V3 0 0 0, V3 0 0 1] blue
                  ]

main :: IO ()
main = display defaultOpts axes
