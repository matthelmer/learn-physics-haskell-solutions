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
--rate :: Int
--rate = 24
--type Position = (Float,Float)
--type Velocity = (Float,Float)
--type State = (Position,Velocity)

--initialState :: State
--initialState = ((0,0),(4,8)) -- position and velocity, each x,y values

--displayFunc :: State -> Gloss.Picture
--displayFunc ((x,y),_) = Gloss.Translate x y redDisk

--updateFunc :: Float -> State -> State
--updateFunc dt ((x,y),(vx,vy))
--    = (( x + 10 * vx * dt, y + 10 * vy * dt)
--      ,(vx          ,vy - 9.8 * dt))

-- main :: IO ()
-- main = Gloss.simulate displayMode Gloss.black rate initialState displayFunc
--        (\_ -> updateFunc)

-------------------
-- * Exercise 13.7
-------------------
-- Try to use simulate to make the red disk oscillate left and right without explicitly giving it an oscillating function like sin or cos.

type Position = (Float,Float)

initialPosition :: Position
initialPosition = (-1,0)

displayFunc :: Position -> Gloss.Picture
displayFunc (x,y) = Gloss.Translate x y redDisk

updateFunc :: Float -> Position -> Position
updateFunc _ (x,y) = ((-x), y)

rate :: Int
rate = 2

main :: IO ()
main = Gloss.simulate displayMode Gloss.black rate initialPosition displayFunc (\_ -> updateFunc)

-------------------
-- * Exercise 13.8
-------------------
-- Rewrite the 3D axes code so that the x-axis points to the right, the y-axis points upward, and the z-axis points out of the page. This is my second favorite coordinate system.

--axes :: VisObject R
--axes = VisObjects [Line Nothing [V3 0 0 0, V3 1 0 0] red
--                  ,Line Nothing [V3 0 0 0, V3 0 1 0] green
--                  ,Line Nothing [V3 0 0 0, V3 0 0 1] blue
--                  ]

--orient :: VisObject R -> VisObject R
--orient pict = RotEulerDeg (Euler 0 0 270) $ pict

--main :: IO ()
--main = display defaultOpts (orient axes)

-------------------
-- * Exercise 13.9
-------------------
-- Modify the rotating cube animation to make the rotation occur clockwise about the x-axis instead of counterclockwise.
--rotatingCube :: Float -> VisObject Float
--rotatingCube t = RotEulerRad (Euler 0 0 (-t)) (Cube 1 Solid blue)

--orient :: VisObject Float -> VisObject Float
--orient pict = RotEulerDeg (Euler 270 180 0) $ pict

--main :: IO ()
--main = animate defaultOpts (orient . rotatingCube)

-------------------
-- * Exercise 13.10
-------------------
-- Write an experimental program, similar to Listing 13-3, using the gloss function simulate to understand how gloss’s simulate uses the update function. Use the same expressions for updateFunc and State that we used in Listing 13-3. You will need to change the values of displayFunc and main. Use a rate of 2 instead of a dt of 0.5. When you run this, you should see that the times passed in by gloss’s simulate are time steps that are all close to 0.5.

--type State = (Int,[Float])

-- updates / sec
--rate :: Int
--rate = 2

--initialState :: State
--initialState = (0,[])

--displayFunc :: State -> Gloss.Picture
--displayFunc (n,ts) = Gloss.Text (show n ++ " " ++ show (take 4 ts))

--updateFunc :: Float -> State -> State
--updateFunc t (n,ts) = (n+1,t:ts)

--main :: IO ()
--main = Gloss.simulate displayMode Gloss.red rate initialState displayFunc (\_ -> updateFunc)
