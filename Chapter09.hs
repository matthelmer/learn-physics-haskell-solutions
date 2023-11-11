module Chapter09 where

type R = Double

------------------
-- * Exercise 9.1
------------------
polarToCart :: (R,R) -> (R,R)
polarToCart (r, theta) = (r * cos theta, r * sin theta)

------------------
-- * Exercise 9.2
------------------
-- Currying and uncurrying are ways of transforming functions such that the function variables or arguments are typed differently, allowing one to modify a function's form as needed.

------------------
-- * Exercise 9.3
------------------
-- prelude function head,
-- head :: [a] => a
-- causes a runtime error if passed an empty list.  Write 'headSafe' to return Nothing if passed empty list, else Just x, where x is head.
headSafe :: [a] -> Maybe a
headSafe xs = case xs of
                []    -> Nothing
                (x:_) -> Just x

------------------
-- * Exercise 9.4
------------------
-- write function to make a list out of a Maybe type.
maybeToList :: Maybe a -> [a]
maybeToList x = case x of
                 Nothing -> []
                 Just x  -> [x]

------------------
-- * Exercise 9.5
------------------
-- Zip utilizes recursion to match the head of both lists into a tuple, until the pattern of an empty list is reached in either or both lists, terminating the recursion and returning the list of tuples.

------------------
-- * Exercise 9.6
------------------
-- Define function zip' that turns a pair of lists into list of pairs (hint: use curry or uncurry)
zip' :: ([a], [b]) -> [(a, b)]
zip' (x,y) = uncurry zip (x,y)

------------------
-- * Exercise 9.7
------------------
-- The dot operator (.) is for function composition. Doing unzip followed by zip', we have a function with type signature:
-- zip' . unzip :: [(a,b)] -> [(a,b)]
-- Is this the identity function? Justify answer. If we do zip' followed by unzip, we have:
-- unzip . zip' :: ([a],[b]) -> ([a],[b])
-- Is this identify function?
-- Answer: In neither case can we be sure that they are identity functions, simply because information may be lost when the input lengths are not the same length.

------------------
-- * Exercise 9.8
------------------
-- show how to use the lookup function to produce the value Just 89, and the value Nothing
grades :: [(String, Int)]
grades = [ ("Albert Einstein", 89)
         , ("Isaac Newton"  , 85)
         , ("Alan Turing"   , 91)
         ]
-- > lookup "Albert Einstein" grades
-- Just 89
-- > lookup "Bob Dylan" grades
-- Nothing

------------------
-- * Exercise 9.9
------------------
-- Translate into haskell:
-- x(r, theta, phi) = r*(sin theta) * (cos phi)
-- Use a triple for the input to function x. Give a type signature as well as function definition.
funcX :: (R, R, R) -> R
funcX (r, theta, phi) = r * (sin theta) * (cos phi)

------------------
-- * Exercise 9.10
------------------
-- A car starts from rest and accelrates 5m/s^2.  Make an infinite list of tvPairs of time-velocity pairs, one per second.
tvPairs :: [(R,R)]
tvPairs = iterate tvUpdate (0,0)
tvUpdate :: (R,R) -> (R,R)
tvUpdate (t,v) = (t+1,v+5)

------------------
-- * Exercise 9.11
------------------
fibHelper :: [(Int,Int)]
fibHelper = iterate (\(a,b) -> (b, a + b)) (0,1)
fibonacci :: [Int]
fibonacci = map snd fibHelper
