{-# OPTIONS -Wall #-}

module SimpleVec where

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
