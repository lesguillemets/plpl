module Plot where
import Prelude hiding (putStr, unlines)
import Data.List hiding (unlines)
import Data.Monoid
import Data.ByteString.Lazy (ByteString, putStr)
import Data.ByteString.Lazy.Builder as BLB

type Range = (Double, Double)
newtype Plot = Plot ByteString
plot :: Double
     -> Double
     -> Int
     -> Int
     -> (Double -> Double)
     -> Plot
plot x0 x1 n h f =
    let dx = (x1-x0) / (fromIntegral n)
        values = map f . take n $ [x0, x0+dx..]
        yMax = maximum values
        yMin = minimum values
        dy = if yMax == yMin
                then 1
                else (yMax - yMin) / fromIntegral h
        cutY y0 = mconcatMap (toDot (y0-dy, y0)) values
        in
    Plot . toLazyByteString . mconcatMap ((<> char7 '\n') . cutY)
        . take (h+2) $ [yMax+dy, yMax..]

mconcatMap :: Monoid c => (a -> c) -> [a] -> c
mconcatMap f = mconcat . map f

draw :: Plot -> IO ()
draw (Plot p) = putStr p

toDot :: Range -> Double -> Builder
toDot (l,h) x
    | x < l =           char7 ' '
    | x <= middleLow  = char7 '.'
    | x <= middleHigh = char7 '*'
    | x <= h =          char7 '^'
    | h < x =           char7 ' '
    | otherwise =       char7 '!'
    where
        third = (h-l) / 3
        middleLow = l + third
        middleHigh = middleLow + third
