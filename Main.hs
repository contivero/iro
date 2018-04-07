module Main where

import Data.Ratio ((%))

import ColorMaps
import Algorithms
import Colors

-- Viridis:
-- https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
main :: IO ()
main = undefined

-- | Naive conversion, without taking color profiles into account.
rgbToCmyk :: RGB -> CMYK
rgbToCmyk (RGB 0 0 0) = CMYK 0 0 0 1
rgbToCmyk (RGB r g b) = CMYK c m y k
  where h = 1 - (fromIntegral r/255)
        i = 1 - (fromIntegral g/255)
        j = 1 - (fromIntegral b/255)
        c = (h - k) / (1 - k)
        m = (i - k) / (1 - k)
        y = (j - k) / (1 - k)
        k = minimum [h,i,j]

-- passes :: ConformanceLevel -> RGB -> RGB -> Bool
passes AA  c1 c2 = contrastRatio c1 c2 >= 4.5
passes AAA c1 c2 = contrastRatio c1 c2 >= 7.0

myList = [x | r <- [0..255], g <- [0..255], b <- [0..255], let x = RGB r g b, let y = contrastRatio white x, y >= 7 && y <= 7.00001]

-- hslToRgb :: (Int, Rational, Rational) -> (Word8, Word8, Word8)
hslToRgb (hue, sat, light)
    | s == 0    = (lumToRgb, lumToRgb, lumToRgb)
    | l <= 0.5  = hslToRgb' h l (l * (s+1))
    | otherwise = hslToRgb' h l (l + s - l*s)
  where h = hue / 360
        s = sat / 100
        l = light / 100
        lumToRgb = round (l * 255)

-- hslToRgb' :: Rational -> Rational -> Rational -> (Word8, Word8, Word8)
hslToRgb' h l t2 = (r, g, b)
  where t1 = l*2 - t2
        r = round $ 255 * hueToRgb t1 t2 (h + (1 % 3))
        g = round $ 255 * hueToRgb t1 t2 h
        b = round $ 255 * hueToRgb t1 t2 (h - (1 % 3))

