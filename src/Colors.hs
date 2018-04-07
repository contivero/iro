
module Colors where

import Data.Word (Word8)

data RGB = RGB
    { _red :: Word8
    , _green :: Word8
    , _blue :: Word8
    } deriving (Eq, Ord, Show)

data CMYK = CMYK
    { _cyan    :: Double
    , _magenta :: Double
    , _yellow  :: Double
    , _key     :: Double
    } deriving (Eq, Show)

data HSL = HSL
    { _hue        :: Double
    , _saturation :: Double
    , _light      :: Double
    } deriving (Eq, Show)


white = RGB 255 255 255
black = RGB 0 0 0
blue = RGB 0 0 255
red = RGB 255 0 0
green = RGB 0 255 0

-- | A color that has the lowest contrast ratio greater than 7 with respect to
-- white, from the viridis color map.
viridisBlue = RGB 56 89 140
