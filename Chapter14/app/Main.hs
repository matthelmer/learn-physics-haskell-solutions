{-# OPTIONS_GHC -Wall #-}

module Main where

type R        = Double
type Mass     = R
type Time     = R
type Position = R
type Velocity = R
type Force    = R

velocityCF :: Mass
           -> Velocity          -- initial velocity
           -> [Force]           -- list of forces
           -> Time -> Velocity  -- velocity function
velocityCF m v0 fs
    = let fNet = sum fs       -- net force
          a0   = fNet / m     -- Newton's 2nd law
          v t  = v0 + a0 * t
      in v
-------------------
-- * Exercise 14.1
-------------------
-- Write a function velocityCF' that does same thing and has same type signature as velocityCF, but in which the time t :: Time is listed on the left of the equal sign in the definition.
velocityCF' :: Mass
            -> Velocity          -- initial velocity
            -> [Force]           -- list of forces
            -> Time -> Velocity  -- velocity function
velocityCF' m v0 fs t
    = let fNet = sum fs       -- net force
          a0   = fNet / m     -- Newton's 2nd law
          v    = v0 + a0 * t
      in v


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Calling velocityCF 1 1 [1] 1..."
  print $ velocityCF 1 1 [1] 1
  putStrLn "Calling velocityCF' 1 1 [1] 1..."
  print $ velocityCF' 1 1 [1] 1
