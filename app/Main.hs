module Main where

import Plot

main :: IO ()
main = draw . plot (Plot sin) $ defaultCfg `withXRange` Range (0, 2*pi)
