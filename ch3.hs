------------------
-- * Exercise 3.1
------------------
{-
__(a) False || True && False || True

Bool
False || ((True && False) || True)

b) 2 / 3 / 4 == 4 / 3 / 2
Bool
((2 / 3) / 4)) == ((4 / 3) / 2)

c) 7 - 5 / 4 > 6 || 2 ^ 5 - 1 == 31
Bool
((7 - (5 / 4)) > 6) || (((2 ^ 5) - 1) == 31)

d) 2 < 3 < 4
Not well-formed
(2 < 3) && (3 < 4)

e) 2 < 3 && 3 < 4
Bool
(2 < 3) && (3 < 4)

f) 2 && 3 < 4
Bool
2 && (3 < 4)
-}

------------------
-- * Exercise 3.2
------------------
--Write haskell func defs for math functions:
-- a)
f :: Double -> Double
f x = if x <= 0
        then 0
        else x
-- b)
e :: Double -> Double
e r = if r <= 1
        then r
        else 1 / r**2

------------------
-- * Exercise 3.3
------------------
-- Define func isXorY that returns True if input is 'X' or 'Y', otherwise False
isXorY :: Char -> Bool
isXorY myChar = case myChar of 
                    'Y' -> True
                    'X' -> True
                    _   -> False

------------------
-- * Exercise 3.4
------------------
-- Def func bagFee that returns 100 if person is checking bags
-- otherwise 0.  use if-else, then do bagFee2 with pattern matching.
bagFee :: Bool -> Int
bagFee checkingBags = if checkingBags
                        then 100
                        else 0
bagFee2 :: Bool -> Int
bagFee2 True = 100
bagFee2 _ = 0

------------------
-- * Exercise 3.5
------------------
greaterThan50 :: Integer -> Bool
greaterThan50 n = n > 50

------------------
-- * Exercise 3.6
------------------
amazingCurve :: Int -> Int
amazingCurve score = case score of
                        greaterThan50 -> 100
                        _             -> 2 * score

------------------
-- * Exercise 3.7
------------------
-- type of expression 'bagFee False': Int
-- value of expression 'bagFee False': 0


------------------
-- * Exercise 3.8
------------------
-- add type signatures
circleRadius :: Double
circleRadius = 3.5

cot :: Double -> Double
cot x = 1 / tan x

fe :: Double -> Double
fe epsilon = epsilon * tan (epsilon * pi / 2)

fo :: Double -> Double
fo epsilon = -epsilon * cot (epsilon * pi / 2)

g :: Double -> Double -> Double
g nu epsilon = sqrt (nu**2 - epsilon**2)


------------------
-- * Exercise 3.9
------------------
-- how many functions have type Bool -> Bool? 4
-- name them?
-- how many have type Bool -> Bool -> Bool?

------------------
-- * Exercise 3.10
------------------
-- devise expression using True, False, && and || that would come out differently if precedence of || was higher than precedence of &&.
-- True || False && False
