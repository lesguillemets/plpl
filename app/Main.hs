module Main where

import Plot

main :: IO ()
main = draw $ plot 0 (2*pi) 60 30 sin
