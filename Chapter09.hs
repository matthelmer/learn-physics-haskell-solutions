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
