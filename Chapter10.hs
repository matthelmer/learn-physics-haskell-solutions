{-# OPTIONS -Wall #-}

module Chapter10 where

import Graphics.Gnuplot.Simple

infixl 6 ^+^
infixl 6 ^-^
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixr 7 ><

type R              = Double
type Time           = R
type PosVec         = Vec
type Velocity       = Vec
type Acceleration   = Vec

type VecDerivative = (R -> Vec) -> R -> Vec
vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt/2) ^-^ v (t - dt/2)) ^/ dt


data Vec = Vec { xComp :: R  -- x component
                ,yComp :: R  -- y component
                ,zComp :: R  -- z component
                } deriving (Eq)

instance Show Vec where
    show (Vec x y z) = "vec " ++ showDouble x ++ " "
                              ++ showDouble y ++ " "
                              ++ showDouble z
showDouble :: R -> String
showDouble x
    | x < 0     = "(" ++ show x ++ ")"
    | otherwise = show x

vec :: R  -- x component
    -> R  -- y component
    -> R  -- z component
    -> Vec
vec = Vec

iHat :: Vec
iHat = vec 1 0 0

jHat :: Vec
jHat = vec 0 1 0

kHat :: Vec
kHat = vec 0 0 1

zeroV :: Vec
zeroV = vec 0 0 0

negateV :: Vec -> Vec
negateV (Vec ax ay az) = Vec (-ax) (-ay) (-az)

(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax+bx) (ay+by) (az+bz)

(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax-bx) (ay-by) (az-bz)

sumV :: [Vec] -> Vec
sumV = foldr (^+^) zeroV

(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c*ax) (c*ay) (c*az)

(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c*ax) (c*ay) (c*az)

(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax*bx + ay*by + az*bz

(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz
    = Vec (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax/c) (ay/c) (az/c)

magnitude :: Vec -> R
magnitude v = sqrt(v <.> v)

-------------------
-- * Exercise 10.1
-------------------
-- Translate following math definitions into Haskell:
-- a) v0 = 20 * iHat
v0 :: Vec
v0 = 20 *^ iHat

-- b) v1 = 20 * iHat - 9.8 * kHat
v1 :: Vec
v1 = 20 *^ iHat ^-^ 9.8 *^ kHat

-- c) v(t) = 20 * iHat - 9.8 * t * kHat
v :: R -> Vec
v t = 20 *^ iHat ^-^ (9.8 * t) *^ kHat

-- d) r(t) = 30 * jHat + 20 * t * iHat - 4.9 * t**2 * kHat
r :: R -> Vec
r t = 30 *^ jHat ^+^ (20 * t) *^ iHat ^-^ (4.9 * t**2) *^ kHat

-- e) x(t) = iHat dotProduct r(t)
x :: R -> R
x t = iHat <.> r t
-- What are the Haskell types of v0, v1, v, r, and x?
-- v0 :: Vec
-- v1 :: Vec
-- v  :: R -> Vec
-- r  :: R -> Vec
-- x  :: R -> R

-------------------
-- * Exercise 10.2
-------------------
-- Write an integration function for vector-valued functions of a real variable, similar to function:
-- integral :: R -> (R -> R) -> R -> R -> R
-- integral dt f a b = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b - dt/2]]
-- that we wrote in chapter 6.
vecIntegral :: R            -- step size dt
            -> (R -> Vec)   -- vector-valued function
            -> R            -- lower limit
            -> R            -- upper limit
            -> Vec          -- result
vecIntegral dt vecFunc a b = sumV [vecFunc t ^* dt | t <- [a+dt/2, a+3*dt/2 .. b - dt/2]]


-------------------
-- * Exercise 10.3
-------------------
-- Write function that returns maximum z-component for projectile motion in which
-- the initial position and initial velocity of object are given. Assume gravity
-- acts in negative z-direction.
-- need to figure out when the vector function derivative is zero

-- given initial position, initial velocity, constant acceleration vector, and time, returns the position vector for time 't'
positionCA :: PosVec -> Velocity -> Acceleration -> Time -> PosVec -- position as a function of time
positionCA r0 v0 a0 t = 0.5 *^ t**2 *^ a0 ^+^ v0 ^* t ^+^ r0

-- given initial position vector, initial velocity vector, returns function positionCA to calculate the position of projectile under earth's gravity at time 't'
projectilePos :: PosVec -> Velocity -> Time -> PosVec
projectilePos r0 v0 = positionCA r0 v0 (9.81 *^ negateV kHat)

-- can test w/ different r0 and v0 values
-- *> maxHeight (Vec 0 0 0) (Vec 0 0 100)
-- *> maxHeight (Vec 10 10 0) (Vec (-10) 5 100)
maxHeight :: PosVec -> Velocity -> R
maxHeight r0 v0 = maximum [zComp (projectilePos r0 v0 t) | t <- [0,0.01..1000]]


-------------------
-- * Exercise 10.4
-------------------
-- Write a function that, given initial velocity and constant acceleration, returns a function giving speed as a function of time.
speedCA :: Velocity -> Acceleration -> Time -> R
speedCA v0 a0 = \t -> magnitude (v0 ^+^ (t *^ a0))

-------------------
-- * Exercise 10.5
-------------------
-- Write a type signature and function definition for projectileVel to compute the velocity of a projectile at a given time.
-- Do in the spirit of projectilePos

-- Position as a function of time (constant accel.):
-- r(t) = 1/2 *^ a0*t^2 + v(0)*t
--
-- Velocity as a function of time (constant accel.):
-- v(t) = a0*t + v(0)

velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 ^* t ^+^ v0
projectileVel :: Velocity -> Time -> Velocity
projectileVel v0 = velocityCA v0 (9.81 *^ negateV kHat)

-------------------
-- * Exercise 10.6
-------------------
-- Define a new type Vec2D for 2-d vectors.
data Vec2D = Vec2D { xComp2D :: R
                    ,yComp2D :: R
                   } deriving (Eq)
instance Show Vec2D where
    show (Vec2D x y) = "vec2D " ++ showDouble x ++ " "
                              ++ showDouble y
vec2D ::   R  -- x component
        -> R  -- y component
        -> Vec2D
vec2D = Vec2D
-- Then define functions to find magnitude and angle of a two-dimensional vector from a magnitude and angle. You may want to use atan or atan2 functions.
magAngleFromVec2D :: Vec2D -> (R,R)
magAngleFromVec2D (Vec2D x y) = (sqrt (x**2 + y**2), atan2 y x)
vec2DFromMagAngle :: (R,R) -> Vec2D
vec2DFromMagAngle (r,theta) = Vec2D (r * cos theta) (r * sin theta)

-------------------
-- * Exercise 10.7
-------------------
-- Define function that computes projection of a vector into the xy plane. For example, xyProj (vec 6 9 7) should evaluate to vec 6 9 0.
xyProj :: Vec -> Vec
xyProj (Vec x y _) = Vec x y 0

-------------------
-- * Exercise 10.8
-------------------
-- Define a function that returns a triple (v, theta, phi) for a vector v in which :
-- v = |v|
-- theta = inv tan
magAngles :: Vec -> (R,R,R)
magAngles (Vec x y z) = (magnitude (Vec x y z), (atan2 (sqrt(x**2 + y**2)) z), (atan2 y x))

-------------------
-- * Exercise 10.9
-------------------
-- Velocity and acceleration of ball launched from the ground are
-- vBall(t) = v0*t + 1/2 * g * t^2
-- aBall(t) = g
-- Suppose ball launched w/ initial speed of 25 m/s at an angle of 52 degrees above horizontal.
-- Choose coordinate system and define constant 9.8m/s^2 acceleration of gravity toward center of earth:
gEarth :: Vec
gEarth = Vec 0 0 (-9.8)

-- next define function that gives velocity of the ball as function of time.
vBall :: R -> Vec
vBall t = let v0 = Vec (25 * cos (52 * pi / 180)) 0 (25 * sin (52 * pi / 180))
          in v0 ^*t ^+^ (0.5 *^ t**2 *^ gEarth)

-- next define function that gives rate of change of speed of the ball as function of time.
-- may want to use speedRateChange for this.
speedRateChange :: Vec -> Vec -> R
speedRateChange v a = (v <.> a) / magnitude v
speedRateChangeBall :: R -> R
speedRateChangeBall t = speedRateChange (vBall t) gEarth
-- At what point in ball's motion is rate of change of its speed equal to zero?  The speed change being equal to zero implies dot product of 'vBall' and 'gEarth' is zero, i.e. they are orthogonal, which will occur at the peak of the ball's motion.
-- Is its's velocity zero at that point?  No, because the velocity vector will have non-zero components due to direction changing.
-- Use plotFunc from chapter 7 to make a graph of the rate of change of the speed of th ball as a function of time over the four seconds it is in the air.
ballSpeedChangePlot :: IO ()
ballSpeedChangePlot = plotFunc [] ([0,0.01..5]) speedRateChangeBall

-------------------
-- * Exercise 10.10
-------------------
-- Consider a particle in uniform circular motion. If motion takes place in xy-plane with the origin at the center of the circle (radius R, angular vel. w),

-- We can write the position of the particle as:
-- r_UCM(t) = R(cos w t iHat + sin w t jHat)

-- Velocity of the particle can be found by taking derivative of position wrt time:
-- v_UCM(t) = w R (-sin w t iHat + cos w t jHat)

-- Acceleration of the particle can be found by taking derivative of velocity wrt time:
-- a_UCM(t) = -w**2 R (cos w t iHat + sin w t jHat)

-- This particle in UCM has speed v_UCM(t) = w*R, which does not depend on time.
-- Using R = 2m, and w = 6 rad/s, encode the following in Haskell:

-- Position:
rUCM :: R -> Vec
rUCM t = 2 *^ (Vec (cos (6 * t)) (sin (6 * t)) 0)

-- Velocity:
vUCM :: R -> Vec
vUCM t = 6 *^ 2 *^ (Vec (-sin (6 * t)) (cos (6 * t)) 0)

-- Acceleration:
aUCM :: R -> Vec
aUCM t = negateV (6**2 *^ 2 *^ (Vec (cos (6 * t)) (sin (6 * t)) 0))

-- Use aParallel to confirm tangential component of acceleration is 0 at different times.
aParallel :: Vec -> Vec -> Vec
aParallel v a = let vHat = v ^/ magnitude v
                in (vHat <.> a) *^ vHat
-- Use aPerp to confirm magnitude of radial compoment of acceleration is [v_UCM(t)]^2 / R = w^2 * R
aPerp :: Vec -> Vec -> Vec
aPerp v a = a ^-^ aParallel v a
-- can confirm by evaluating:
-- magnitude(aPerp (vUCM 1) (aUCM 1)) is equal to 72

-------------------
-- * Exercise 10.11
-------------------
-- Non-uniform Circular Motion, radius R, xy-plane
-- Position of particle, where theta t gives angle particle makes with x-axis at time, t:
-- rNCM(t) = R[cos theta t ihat + sin theta t jhat]
rNCM :: (R, R -> R) -> R -> Vec
rNCM (radius, theta) t = radius *^ (Vec (cos (theta t)) (sin (theta t)) 0)

-- Use aPerpFromPosition, which finds radial component of acceleration for a particle whose position can be given as function of time,
aPerpFromPosition :: R -> (R -> Vec) -> R -> Vec
aPerpFromPosition epsilon r t
    = let v = vecDerivative epsilon r
          a = vecDerivative epsilon v
      in aPerp (v t) (a t)

-- for radius R = 2m, and
-- theta(t) = 1/2 * (3 rad / s**2) * t**2
thetaNCM = \t -> (1 / 2) * 3 * t**2

-- use aPerpFromPosition to find radial component of acceleration at t=2 seconds.
-- evaluate in ghci as:
-- SimpleVec> aPerpFromPosition 0.01 (rNCM (2, thetaNCM)) 2
-- vec (-69.11249319950575) 20.109352256620774 0.0

-- Find the speed of the particle at that time.
-- Let's use velocity derivative of the position
speedAtTime :: R -> (R, R -> R) -> R -> R
speedAtTime epsilon (radius, theta) t
    = let vd = vecDerivative epsilon (rNCM (radius, theta))
      in magnitude (vd t)

-- evaluate the speed at t=2:
-- speedAtTime 0.01 (2, thetaNCM) 2
-- Finally, show that the magnitude of the radial component is equal to the square of its speed divided by the radius of the circle.
checkRadialAccel :: R -> (R, R -> R) -> R -> Bool
checkRadialAccel epsilon (radius, theta) t
    = let radialAccelVec = aPerpFromPosition epsilon (rNCM (radius, theta)) t
                   speed = speedAtTime epsilon (radius, theta) t
          radialAccelMag = magnitude radialAccelVec
      in radialAccelMag == (speed**2) / radius
-- checkRadialAccel 0.01 (2, thetaNCM) 2
