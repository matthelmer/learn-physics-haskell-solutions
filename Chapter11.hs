{-# OPTIONS_GHC -Wall #-}

import Graphics.Gnuplot.Simple
type R = Double

tRange :: [R]
tRange = [0,0.01..5]

yPos :: R  -- y0
     -> R  -- vy0
     -> R  -- ay
     -> R  -- t
     -> R  -- y
yPos y0 vy0 ay t = y0 + vy0 * t + ay * t**2 / 2

plot1 :: IO ()
plot1 = plotFunc [Title "Projectile Motion"
                 ,XLabel "Time (s)"
                 ,YLabel "Height of projectile (m)"
                 ,PNG "projectile.png"
                 ,Key Nothing
                 ] tRange (yPos 0 20 (-9.8))


-------------------
-- * Exercise 11.1
-------------------
-- Make plot of y = x^2 from x = -3 to x = 3 with a title and axis labels.
xRange :: [R]
xRange = [-3,-2.9..3]

y :: R -> R
y x = x**2

plotEx1 :: IO ()
plotEx1 = plotFunc [Title "Exercise 11.1: y = x^2"
                    ,XLabel "x"
                    ,YLabel "y"
                    ,PNG "exercise11-1.png"
                    ,Key Nothing
                    ] xRange y

-------------------
-- * Exercise 11.2
-------------------
-- Make a plot of the cosine and sine functions, together on a single set of axes, from x=0 to x=10
x2Range :: [R]
x2Range = [0,0.001..1009]

plotEx2 :: IO ()
plotEx2 = plotFuncs [] x2Range [cos,sin]

-------------------
-- * Exercise 11.3
-------------------
-- Figure out how to plot the list of points txPairs below.  Make plot with a title and axis labels (with units).
ts :: [R]
ts = [0,0.1..6]

txPairs :: [(R,R)]
txPairs = [(t,30 * t - 4.99 * t**2) | t <- ts]

plotEx3 :: IO ()
plotEx3 = plotPath [Title "Exercise 11.3"
                   ,XLabel "t"
                   ,YLabel "value"
                   ,Key Nothing
                   ] txPairs


-------------------
-- * Exercise 11.4
-------------------
-- Write a function that approximates the sine function by the first four terms in its Taylor expansion. Add axis labels, title, etc.
-- x - (x^3 / 3!) + (x^5 / 5!) - (x^7 / 7!)
fact :: Integer -> R
fact n = fromIntegral(product [n,n-1..1])

approxsin :: R -> R
approxsin x = x - x**3 / (fact 3) + x**5 / (fact 5) - x**7 / (fact 7)

plotEx4 :: IO ()
plotEx4 = plotFuncs [] [-4,-3.99..4] [sin, approxsin]
