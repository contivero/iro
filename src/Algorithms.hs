module Algorithms where

-- import Data.Foldable (maximum, minimum)
import Data.Word
import qualified Data.Vector as V
import Colors
import Data.Ratio ((%))

-- contrast ratios range from 1 (1:11), to 21 (21:1)
-- https://www.w3.org/TR/WCAG/#visual-audio-contrast
-- Formulas defined by Gregg Vanderheiden at Trade Center and Dave Kelso

contrastRatio :: RGB -> RGB -> Double
contrastRatio c1 c2 =  x / 10
  where l1 = luminance c1
        l2 = luminance c2
        x  = fromIntegral $  round ((max l1 l2 + 0.05) / (min l1 l2 + 0.05) * 10)

data ConformanceLevel = AA  -- minimum
                      | AAA -- enhanced
  deriving (Eq, Ord, Show)

findClosestTo n col colormap = V.minimum . V.map f $ V.filter g colormap
  where f x = (abs $ contrastRatio col x - n, x)
        g x = contrastRatio white x >= 7

-- | Calculate greyscale through a 'lightness' formula
lightness :: RGB -> Double
lightness (RGB r g b) = (fromIntegral $ lighter + darker) / 2
  where lighter = maximum [r,g,b]
        darker  = minimum [r,g,b]

-- | Relative luminance for RGB, as specified in: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
-- This has several problems, see e.g. http://entropymine.com/imageworsener/srgbformula/

luminance (RGB r' g' b') = 0.2126 * r + 0.7152 * g + 0.0722 * b
  where linearRGB x
          | x <= 0.03928 = x / 12.92
          | otherwise    = ((x + 0.055) / 1.055) ** 2.4
        r = linearRGB (fromIntegral r' / 255)
        g = linearRGB (fromIntegral g' / 255)
        b = linearRGB (fromIntegral b' / 255)

csrgb :: Double -- Clinear
      -> Double
csrgb c
    | c <= 0.0031308 = 12.92 * c
    | otherwise      = (1+a) * c ** (1/2.4) - 0.055
  where a = 0.055

-- | Calculate greylevel as an average
average :: RGB -> Double
average (RGB r g b) = (r' + g' + b') / 3
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

-- https://www.w3.org/TR/AERT#color-contrast
-- For problems with these guidelines, see: http://colaargh.blogspot.com.ar/2013/11/holes-in-w3c-colour-readability.html
-- YIQ Brightness
colorBrightness :: RGB -> Double
colorBrightness (RGB r' g' b') = (r * 299.0 + g * 587.0 + b * 114.0)/1000
  where r = fromIntegral r'
        g = fromIntegral g'
        b = fromIntegral b'

-- rough-n-ready formula
brightnessValue :: RGB -> Double
brightnessValue (RGB r' g' b') = (0.22475 * r**a + 0.7154 * g**a + 0.05575 * b**a) ** invP
  where invP = 1 / 2.235
        a = 2.235
        r = fromIntegral r'
        g = fromIntegral g'
        b = fromIntegral b'


-- 0 = no difference in color
-- 765 = maximum difference in color (i.e. black and white)
colorDifference :: RGB -> RGB -> Double
colorDifference (RGB r1 g1 b1) (RGB r2 g2 b2) = redDiff + greenDiff + blueDiff
  where diff x y  = abs (fromIntegral x - fromIntegral y) :: Double
        redDiff   = diff r1 r2
        greenDiff = diff g1 g2
        blueDiff  = diff b1 b2

hueToRgb :: Rational -> Rational -> Rational -> Rational
hueToRgb t1 t2 hue
    | hue < 0   = test t1 t2 (hue+1)
    | hue > 1   = test t1 t2 (hue-1)
    | otherwise = test t1 t2 hue
  where test :: Rational -> Rational -> Rational -> Rational
        test a b h
            | h * 6 < 1 = a + (b-a) * 6 * h
            | h * 2 < 1 = b
            | h * 3 < 2 = a + (b-a) * ((2 % 3) - h) * 6
            | otherwise = a

sRGBtoXYZ =
  [[0.4124564, 0.3575761, 0.1804375]
  ,[0.2126729, 0.7151522, 0.0721750]
  ,[0.0193339, 0.1191920, 0.9503041]]

