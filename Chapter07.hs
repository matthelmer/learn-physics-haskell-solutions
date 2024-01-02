module Chapter07 where

import Chapter02 (yRock30)
import Chapter06 (yRock)
import Graphics.Gnuplot.Simple

type R = Double

------------------
-- * Exercise 7.1
------------------
plot1 :: IO ()
plot1 = plotFunc [] ([-10,-9.9..10] :: [R]) sin

------------------
-- * Exercise 7.2
------------------
plot2 :: IO ()
plot2 = plotFunc [] [-10,-9.9..1000] yRock30

------------------
-- * Exercise 7.3
------------------
plot3 :: IO ()
plot3 = plotFunc [] [0,0.1..4] (yRock 20)
