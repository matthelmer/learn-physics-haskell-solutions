module Chapter02 where
------------------
-- * Exercise 2.1
------------------
f :: Floating a => a -> a
f x = sqrt (1 + x)


------------------
-- * Exercise 2.2
------------------
-- acceleration due to gravity, m/s/s
earthG :: Double
earthG = 9.807

-- rock with initial velocity 30 m/s upward
yRock30 :: Double -> Double
yRock30 t = 0.5 * (-earthG) * t ** 2 + 30 * t


------------------
-- * Exercise 2.3
------------------
-- output upward velocity of rock in m/s
vRock30 :: Double -> Double
vRock30 t = 30 + (-earthG) * t


------------------
-- * Exercise 2.4
------------------
-- computes sine of an angle given in degrees
sinDeg :: Double -> Double
sinDeg theta = sin $ theta * pi / 180


------------------
-- * Exercise 2.5
------------------
-- haskell for math functions
f' :: Double -> Double
f' x = x ** (1/3)

g :: Double -> Double
g y = exp y + 8 ** y

h :: Double -> Double
h x = 1 / (sqrt ((x - 5)**2 + 16))

y :: Double -> Double
y b = 1 / (sqrt (1 - b**2))

u :: Double -> Double
u x = 1 / (10 + x) + 1 / (10 - x)

l :: Double -> Double
l x = sqrt (x * (x + 1))

e' :: Double -> Double
e' x = 1 / (abs x)**3

e'' :: Double -> Double
e'' z = 1 / (z**2 + 4)**(3/2)


------------------
-- * Exercise 2.6
------------------
-- lambda for gamma function
gamma = \b -> 1 / (sqrt (1 - b**2))
-- expression 
{-
Main> (\b -> 1 / (sqrt (1 - b**2)) 0.8
1.66666666667
-}
