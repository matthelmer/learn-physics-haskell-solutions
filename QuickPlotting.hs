import Graphics.Gnuplot.Simple

type R = Double

-- Here we define a constant
e :: Double
e = exp 1

-- Here we define a function
square :: R -> R
square x = x**2

plot1 :: IO ()
plot1 = plotFunc [] [-3,-2.99..3] square
