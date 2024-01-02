module Chapter05 where

type R = Double

------------------
-- * Exercise 5.1
------------------
numbers :: [R]
numbers = [-2.0, -1.2..2.0]

------------------
-- * Exercise 5.2
------------------
sndItem0 :: [a] -> a
sndItem0 xs = if null xs
              then error "Empty list has no second element."
              else if length xs == 1
                   then error "1-item list has no 2nd item."
                   else xs !! 1

------------------
-- * Exercise 5.3
------------------
-- What is the type of expression: length "Hello, world!"
-- Int
-- What is the value of the expression?
-- 13


------------------
-- * Exercise 5.4
------------------
-- Write a function with type Int -> [Int] and describe what it does.
-- myFunc takes an integer argument as input and returns a list consisting of the integer repeated a number of times equal to its value
myFunc :: Int -> [Int]
myFunc n = take n (repeat n)

------------------
-- * Exercise 5.5
------------------
-- Write function null' that does same thing as Prelude function null. Use Prelude function length in your definition of null' but do not use null
null' :: [a] -> Bool
null' as = length as == 0

------------------
-- * Exercise 5.6
------------------
-- Write function last' that does same thing as Prelude function last. Use Prelude functions head and reverse in your definition of last' but do not use the function last.
last' :: [a] -> a
last' as = if null as
           then error "Empty list has no last element"
           else head (reverse as)

------------------
-- * Exercise 5.7
------------------
-- Write function palindrome :: String -> Bool that returns True if input string is a palindrome and False otherwise
palindrome :: String -> Bool
palindrome word = reverse word == word

------------------
-- * Exercise 5.8
------------------
-- What are first five elements of infinite list [9,1..]?
-- [9, 1, -7, -15, -23]

------------------
-- * Exercise 5.9
------------------
-- write function cycle' (same as cycle from Prelude). Use repeat and concat in cycle'.
cycle' :: [a] -> [a]
cycle' [] = error "Empty list cannot be cycled"
cycle' as = concat (repeat as)

------------------
-- * Exercise 5.10
------------------
-- Which of following are valid haskell expressions? if valid, give the type.  if not valid, explain.
-- a) ["hello", 42] is not valid because of different types
-- b) ['h', "ello"] is not valid because 2nd element is a list of chars
-- c) ['a', 'b', 'c'] is valid and has type [Char]
-- d) length ['w', 'h', 'o'] is valid and has type Int
-- e) length "hello" is valid and has type Int
-- f) reverse is valid and has type [a] -> [a]

------------------
-- * Exercise 5.11
------------------
-- in an arithmetic sequence, if specified last element does not occur in the sequence, the sequence will terminate prior to the last element if it is not a whole number, and it is greater than half of the sequence step size, so for example
-- [0,3..7.5] evaluates to [0.0,3.0,6.0,9.0]
-- but [0,3..7.4] evaluates to [0.0,3.0,6.0]

------------------
-- * Exercise 5.12
------------------
-- Euler showed that sum of 1/n^2 over n=1 to infinity is equal to pi^2 / 6
-- Write haskell expression to evaluate sum from n=1 to 100 of 1 / n^2
eulerSum :: R
eulerSum = sum [1 / n**2 | n <- [1,2..100]]

------------------
-- * Exercise 5.13
------------------
-- n! or n factorial written using product function
fact :: Integer -> Integer
fact n = product [n,n-1..1]

------------------
-- * Exercise 5.14
------------------
-- exp(x) = lim (1 + x/n)**n as n approaches infinity
-- Write function that takes a real as input and produces infinite list
-- of successive approximations to exp(x):
-- [(1 + x/1)**1, (1 + x/2)**2, (1 + x/3)**3, ...]
-- How big does n need to be to get within 1% of correct value for x=1?
-- How big does n need to be to get within 1% of correct value for x=10?
expList :: R -> [R]
expList x = [(1 + x / n) ** n | n <- [1..]]

------------------
-- * Exercise 5.15
------------------
expSeries :: R -> [R]
expSeries x = [x ** m / fromIntegral (fact (fromIntegral m)) | m <- [0,1..]]

