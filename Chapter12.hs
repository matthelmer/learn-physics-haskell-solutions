{-# OPTIONS_GHC -Wall #-}


-------------------
-- * Exercise 12.1
-------------------
-- Write a stand-alone program that prints the first 21 powers of 2, from 2^0, ending with 2^20.  Running program should look like this:
-- [1,2,3,8,16, ..., 1048576]
main :: IO ()
main = let pow2 = take 21 [2^n | n <- [0,1..]]
       in print pow2
