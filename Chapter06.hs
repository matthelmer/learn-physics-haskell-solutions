module Chapter06 where

type R = Double

------------------
-- * Exercise 6.1
------------------
earthG :: R
earthG = 9.807

yRock :: R -> R -> R
yRock v0 t = 0.5 * (-earthG) * t ** 2 + v0 * t

vRock :: R -> R -> R
vRock v0 t = v0 - earthG * t

------------------
-- * Exercise 6.2
------------------
-- Give the type of take 4
-- > take 4 :: [a] -> [a]

------------------
-- * Exercise 6.3
------------------
-- :t map not
-- map not :: [Bool] -> [Bool]

------------------
-- * Exercise 6.4
------------------
-- greaterThanOrEq7 n = if n >= 7 then True else False
-- rewrite without an if-then 
greaterThanOrEq7' :: Int -> Bool
greaterThanOrEq7' n = n >= 7

------------------
-- * Exercise 6.5
------------------
-- return True if String is at least length 'n'
funcEx5 :: Int -> String -> Bool
funcEx5 n word = length word >= n

------------------
-- * Exercise 6.6
------------------
-- predicate expressing property "has more than six elements" 
moreThan6 :: [a] -> Bool
moreThan6 as = length as > 6

------------------
-- * Exercise 6.7
------------------
-- explain why replicate 3 'x' returns "xxx", a seemingly different type than Int -> a -> [a]
-- > "xxx" is actually a list of Chars

------------------
-- * Exercise 6.8
------------------
-- make a list of the first 1000 squares. 
-- [x ** 2 | x <- [1..1000]]

------------------
-- * Exercise 6.9
------------------
-- use iterate to define a function repeat' that does same thing as repeat
repeat' :: a -> [a]
repeat' a = iterate (\a -> a) a

------------------
-- * Exercise 6.10
------------------
-- use take and repeat to define function replicate' to do same as replicate
replicate' :: Int -> a -> [a]
replicate' n a = take n (repeat a)

------------------
-- * Exercise 6.11
------------------
-- car from rest accelerates at 5 m/s**2
-- use iterate to make infinite list of velocities for this car, 
-- with one velocity every second.  use take function to see first few times.
-- take 5 (iterate (+5) 0)

------------------
-- * Exercise 6.12
------------------
-- write function map' that does same thing as map, using list comprehension
map' :: (a -> b) -> [a] -> [b]
map' f as = [f a | a <- as]

------------------
-- * Exercise 6.13
------------------
-- write function filter' that does same thing as filter, via list comprehension
filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate as = [a | a <- as, predicate a]

------------------
-- * Exercise 6.14
------------------
average :: [R] -> R
average xs = sum xs / fromIntegral (length xs)

------------------
-- * Exercise 6.15
------------------
-- draw one-input and two-input diagrams for "drop" and "replicate"

------------------
-- * Exercise 6.16
------------------
trapIntegrate :: Int        -- # of trapezoids n
              -> (R -> R)   -- function f
              -> R          -- lower limit a
              -> R          -- upper limit b
              -> R          -- result
trapIntegrate n f a b = (0.5 * f a + 0.5 * f b + sum[f (a + delta * k) | k <- [1..fromIntegral(n-1)]]) * delta 
    where delta = (b - a) / fromIntegral n
