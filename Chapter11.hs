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
