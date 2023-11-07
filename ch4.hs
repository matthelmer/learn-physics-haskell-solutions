module Chapter04 where

type R = Double

-- a derivative is a function from a real to a real that yields
-- another function from a real to a real
type Derivative = (R -> R) -> R -> R

derivative :: R -> Derivative
derivative dt x t = (x (t + dt / 2) - x (t - dt / 2)) / dt

------------------
-- * Exercise 4.1
------------------
f41 :: R -> R
f41 x = 1 / 2 * x ** 2
-- > derivative 0.1 f41 1
-- 1.0000000000000002


------------------
-- * Exercise 4.2
------------------
f42 :: R -> R
f42 x = x ** 3

df42 :: R -> R
df42 x = 3 * x ** 2

-- returns error between numerical derivative est. and analytical derivative
errF :: 
  -- original function
  (R -> R) ->
  -- analytical derivative function
  (R -> R) ->
  -- delta "dt"
  R ->
  -- x value of function
  R ->
  -- return the error
  R
errF f df a x = abs $ derivative a f x - df x

-- error function for f42
err42 ::
  -- delta "dt" value
  R ->
  -- x value
  R ->
  -- error
  R
err42 = errF f42 df42

errA ::
  -- delta value "a"
  R ->
  -- error
  R
errA a = a ** 2 / 4


------------------
-- * Exercise 4.4
------------------
-- for what values of the independent variable 't' is numerical derivative of cosine function 'cos', or 'derivative a cos', most sensitive to the value of 'a'? least sensitive?
