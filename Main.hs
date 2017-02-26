module Main where

import Data.Ratio ((%))

import ColorMaps

-- Viridis:
-- https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
main :: IO ()
main = undefined

data CMYK = CMYK { cyan :: Double
                 , magenta :: Double
                 , yellow :: Double
                 , key :: Double
                 } deriving (Eq, Show)

data HSL = HSL { hue :: Double
               , saturation :: Double
               , light :: Double
               } deriving (Eq, Show)

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

passes :: ConformanceLevel -> RGB -> RGB -> Bool
passes AA c1 c2 = contrastRatio c1 c2 >= 4.5
passes AAA c1 c2 = contrastRatio c1 c2 >= 7


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

{-
function checkcontrast() {
	var normal = document.getElementById("normal");
	var big = document.getElementById("big");
	var contrastratio = document.getElementById("contrastratio");
	var normalaa = document.getElementById("normalaa");
	var normalaaa = document.getElementById("normalaaa");
	var bigaa = document.getElementById("bigaa");
	var bigaaa = document.getElementById("bigaaa");
	var fg = document.getElementById("fg");
	var bg = document.getElementById("bg");

	var color=getColor("foreground");
	var bgcolor=getColor("background");

	var L1 = getL(color);
	var L2 = getL(bgcolor);

	if (L1!==false && L2!==false) {
		normal.style.color = "#"+color;
		normal.style.backgroundColor = "#"+bgcolor;
		big.style.color = "#"+color;
		big.style.backgroundColor = "#"+bgcolor;
		fg.style.backgroundColor = "#"+color;
		bg.style.backgroundColor = "#"+bgcolor;
		var ratio = (Math.max(L1, L2) + 0.05)/(Math.min(L1, L2) + 0.05);
		contrastratio.innerHTML = (Math.round(ratio*100)/100) + ":1";
		if(ratio >= 4.5) {
			normalaa.innerHTML = "Pass";
			normalaa.className='pass';
			bigaaa.innerHTML = "Pass";
			bigaaa.className="pass";
		}
		else {
			normalaa.innerHTML = "Fail";
			normalaa.className="fail";
			bigaaa.innerHTML = "Fail";
			bigaaa.className="fail";
		}
		if(ratio >= 3) {
			bigaa.innerHTML = "Pass";
			bigaa.className='pass';
		}
		else {
			bigaa.innerHTML = "Fail";
			bigaa.className='fail';
		}
		if(ratio >= 7) {
			normalaaa.innerHTML = "Pass";
			normalaaa.className='pass';
		}
		else {
			normalaaa.innerHTML = "Fail";
			normalaaa.className='fail';
		}
	}
	else {
		normal.style.color = "#00f";
		normal.style.backgroundColor = "#fff";
		big.style.color = "#00f";
		big.style.backgroundColor = "#fff";
		contrastratio.innerHTML = "N/A";
		normalaa.innerHTML = "?";
		normalaaa.innerHTML = "?";
		bigaa.innerHTML = "?";
		bigaaa.innerHTML = "?";
	}
}
-}
