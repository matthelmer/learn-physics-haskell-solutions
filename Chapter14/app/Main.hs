{-# OPTIONS_GHC -Wall #-}

module Main where

type R = Double

-------------------
-- * Exercise 14.1
-------------------
-- Write a function velocityCF' that does same thing and has same type signature as velocityCF, but in which the time t :: Time is listed on the left of the equal sign in the definition.
-- velocityCF' :: Mass
--             -> Velocity          -- initial velocity
--             -> [Force]           -- list of forces
--             -> Time -> Velocity  -- velocity function
-- velocityCF' m v0 fs t = undefined m v0 fs t
