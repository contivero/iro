module Algorithms where

import Data.Word

-- contrast ratios range from 1 (1:11), to 21 (21:1)
-- https://www.w3.org/TR/WCAG/#visual-audio-contrast
-- Formulas defined by Gregg Vanderheiden at Trade Center and Dave Kelso
luminocityContrastRatio c1 c2 = contrastRatio' (max l1 l2) (min l1 l2)
  where l1 = luminance c1
        l2 = luminance c2
        contrastRatio' :: Double -- relative luminance of the lighter of the colors
                       -> Double -- relative luminance of the darker of the colors
                       -> Double
        contrastRatio' lighterLum darkerLum  = (lighterLum + 0.05) / (darkerLum + 0.05)

data ConformanceLevel = AA -- minimum
                      | AAA -- enhanced
  deriving (Eq, Ord, Show)


white = RGB 255 255 255

-- A color that has the lowest contrast ratio greater than 7 with respect to
-- white, from the viridis color map.
viridisBlue = RGB 56 89 140

findClosestTo n col colormap = V.minimum . V.map f $ V.filter g colormap
  where f x = (abs $ contrastRatio col x - n, x)
        g x = contrastRatio white x >= 7

data RGB = RGB { red :: Word8
               , green :: Word8
               , blue :: Word8
               } deriving (Eq, Ord, Show)

-- Calculate greyscale through a 'lightness' formula
lightness :: RGB -> Double
lightness (RGB r g b) = 0.5 * (maximum [r,g,b] + minimum [r,g,b])

-- Clinear
luminosity :: RGB -> Double
luminosity (RGB r g b) = 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

csrgb :: Double -- Clinear
      -> Double
csrgb c
    | c <= 0.0031308 = 12.92 * c
    | otherwise      = (1+a) * c ** (1/2.4) - 0.055
  where a = 0.055

-- Calculate greylevel as an average
average :: RGB -> Double
average (RGB r g b) = (r' + g' + b') / 3
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

-- https://www.w3.org/TR/AERT#color-contrast
-- For problems with these guidelines, see: http://colaargh.blogspot.com.ar/2013/11/holes-in-w3c-colour-readability.html
-- YIQ Brightness
colorBrightness :: RGB -> Double
colorBrightness (RGB r g b) = (r * 299 + g * 587 + b * 114)/1000

-- rough-n-ready formula
brightnessValue :: RGB -> Double
brightnessValue (RGB r g b) = (0.22475 * r**a + 0.7154 * g**a + 0.05575 * b**a) ** invP
  where invP = 1 / 2.235
        a = 2.235


-- 0 = no difference in color
-- 765 = maximum difference in color (i.e. black and white)
colorDifference :: RGB -> RGB -> Double
colorDifference (RGB r1 g1 b1) (RGB r2 g2 b2) = redDiff + greenDiff + blueDiff
  where diff x y  = abs (fromIntegral x - fromIntegral y) :: Double
        redDiff   = diff r1 r2
        greenDiff = diff g1 g2
        blueDiff  = diff b1 b2


-- As specified in: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
-- This has several problems, see e.g. http://entropymine.com/imageworsener/srgbformula/
luminance :: (Ord a, Floating a) => RGB -> a
luminance (RGB r g b) = 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
  where r' | normalizedR <= 0.03928 = normalizedR / 12.92
           | otherwise              = ((normalizedR + a) / (1+a)) ** 2.4
        g' | normalizedG <= 0.03928 = normalizedG / 12.92
           | otherwise              = ((normalizedG + a) / (1+a)) ** 2.4
        b' | normalizedB <= 0.03928 = normalizedB / 12.92
           | otherwise              = ((normalizedB + a) / (1+a)) ** 2.4
        normalizedR = fromIntegral r / 255
        normalizedG = fromIntegral g / 255
        normalizedB = fromIntegral b / 255
        a = 0.055

