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
getRangeDefault _ (Range r) = r
getRangeDefault def Unspecified = def

newtype Plot = Plot (Double -> Double)

data PlotCfg = PlotCfg { name :: Maybe String
                       , xRange :: Range
                       , yRange :: Range
                       , xN :: Int
                       , yN :: Int }

defaultCfg :: PlotCfg
defaultCfg = PlotCfg Nothing (Range ((-1), 1)) Unspecified 60 30
withXRange :: PlotCfg -> Range -> PlotCfg
withXRange p x = p{xRange = x}
withYRange :: PlotCfg -> Range -> PlotCfg
withYRange p y = p{yRange = y}
withWidth :: PlotCfg -> Int -> PlotCfg
withWidth p w = p{xN = w}
withHeight :: PlotCfg -> Int -> PlotCfg
withHeight p h = p{yN = h}

plt :: Plot -> PlotCfg -> (PlotImg, PlotCfg)
plt (Plot func) c@(PlotCfg {..}) =
    let (x0, x1) = getRangeDefault ((-1), 1) xRange
        dx = (x1-x0) / (fromIntegral xN)
        values = map func . take xN $ [x0, x0+dx..]
        (yMax, yMin) = getRangeDefault (maximum values, minimum values) yRange
        dy = if yMax == yMin
                then 1
                else (yMax - yMin) / fromIntegral yN
        cutY y0 = mconcatMap (toDot (y0-dy, y0)) values
        img = PlotImg . toLazyByteString . mconcatMap ((<> char7 '\n') . cutY)
                . take (yN+2) $ [yMax+dy, yMax..]
        in
    (img, c{yRange = Range (yMax, yMin)})

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
