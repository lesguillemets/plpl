module Plot where
import Data.List

type Range = (Double, Double)
plot :: Double
     -> Double
     -> Int
     -> Int
     -> (Double -> Double)
     -> [String]
plot x0 x1 n h f =
    let dx = (x1-x0) / (fromIntegral n)
        values = map f . take n $ [x0, x0+dx..]
        yMax = maximum values
        yMin = minimum values
        dy = if yMax == yMin
                then 1
                else (yMax - yMin) / fromIntegral h
        cutY y0 = map (toDot (y0-dy, y0)) values
        in
    map cutY . take (h+2) $ [yMax+dy, yMax..]

toDot :: Range -> Double -> Char
toDot (l,h) x
    | x < l =           ' '
    | x <= middleLow  = '.'
    | x <= middleHigh = '*'
    | x <= h =          '^'
    | h < x =           ' '
    | otherwise =       '!'
    where
        third = (h-l) / 3
        middleLow = l + third
        middleHigh = middleLow + third
