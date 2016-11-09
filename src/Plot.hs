module Plot where
import Data.List

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
        cutY y0 = map (toDot . (\v -> y0-dy < v && v <= y0)) values
        in
    map cutY . take (h+2) $ [yMax+dy, yMax..]

toDot True = '*'
toDot False = ' '


