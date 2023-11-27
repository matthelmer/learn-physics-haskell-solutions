{-# OPTIONS -Wall #-}

module SimpleVec where

infixl 6 ^+^
infixl 6 ^-^
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 7 <.>
infixr 7 ><

type R = Double

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
v0 = 20 *^ iHat

-- b) v1 = 20 * iHat - 9.8 * kHat
v1 = 20 *^ iHat ^-^ 9.8 *^ kHat

-- c) v(t) = 20 * iHat - 9.8 * t * kHat
v t = 20 *^ iHat ^-^ (9.8 * t) *^ kHat

-- d) r(t) = 30 * jHat + 20 * t * iHat - 4.9 * t**2 * kHat
r t = 30 *^ jHat ^+^ (20 * t) *^ iHat ^-^ (4.9 * t**2) *^ kHat

-- e) x(t) = iHat dotProduct r(t)
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
-- that we wrote in chapter 6.
vecIntegral :: R            -- step size dt
            -> (R -> Vec)   -- vector-valued function
            -> R            -- lower limit
            -> R            -- upper limit
            -> Vec          -- result
vecIntegral = undefined


-------------------
-- * Exercise 10.3
-------------------

-------------------
-- * Exercise 10.4
-------------------
-------------------
-- * Exercise 10.5
-------------------
-------------------
-- * Exercise 10.6
-------------------
-------------------
-- * Exercise 10.7
-------------------
-------------------
-- * Exercise 10.8
-------------------
-------------------
-- * Exercise 10.9
-------------------
-------------------
-- * Exercise 10.1
-------------------
-------------------
-- * Exercise 10.11
-------------------
