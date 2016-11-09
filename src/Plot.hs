{-# LANGUAGE RecordWildCards #-}
module Plot( PlotImg
           , Plot (..)
           , Range (..)
           , plot
           , plt
           , draw
    ) where

import Prelude hiding (putStr, unlines)
import Data.List hiding (unlines)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (ByteString, putStr)
import Data.ByteString.Lazy.Builder as BLB

newtype PlotImg = PlotImg ByteString

data  Range = Range (Double, Double)
            | Unspecified
instance Show Range where
    show (Range (t0,t1)) = "from " <> show t0 <> " to " <> show t1
    show Unspecified = "unspecified"

unspecified :: Range -> Bool
unspecified Unspecified = True
unspecified _ = False

getRangeDefault :: (Double,Double) -> Range -> (Double, Double)
getRangeDefault def Unspecified = def
getRangeDefault _ (Range r) = r

data Plot = Plot { func :: Double -> Double
                 , name :: Maybe String
                 , xRange :: Range
                 , yRange :: Range
                 , xN :: Int
                 , yN :: Int }
instance Show Plot where
    show (Plot {..})
        = (fromMaybe "" $ (<> ":") <$> name)
        <>  " x " <> show xRange <> " y " <> show yRange

plt :: Plot -> PlotImg
plt p@(Plot {..}) =
    let (x0, x1) = getRangeDefault ((-1), 1) xRange
        dx = (x1-x0) / (fromIntegral xN)
        values = map func . take xN $ [x0, x0+dx..]
        yMax = maximum values
        yMin = minimum values
        dy = if yMax == yMin
                then 1
                else (yMax - yMin) / fromIntegral yN
        cutY y0 = mconcatMap (toDot (y0-dy, y0)) values
        in
    PlotImg . toLazyByteString . mconcatMap ((<> char7 '\n') . cutY)
        . take (yN+2) $ [yMax+dy, yMax..]

plot :: Double
     -> Double
     -> Int
     -> Int
     -> (Double -> Double)
     -> PlotImg
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
    PlotImg . toLazyByteString . mconcatMap ((<> char7 '\n') . cutY)
        . take (h+2) $ [yMax+dy, yMax..]

mconcatMap :: Monoid c => (a -> c) -> [a] -> c
mconcatMap f = mconcat . map f

draw :: PlotImg -> IO ()
draw (PlotImg p) = putStr p

toDot :: (Double,Double) -> Double -> Builder
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
