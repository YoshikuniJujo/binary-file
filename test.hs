{-# LANGUAGE QuasiQuotes #-}

import QuoteBinaryStructure
import System.Environment
import Data.Bits

[binary|

BitmapFileHeader

2: 19778
4: fileSize
2: 0
2: 0
4: offset
4: 40
4: bitmapWidth
4: bitmapHeight
2: 1
2: bitPerPic
4: compress
4: imageDataSize
4: horizontalDensity
4: verticalDensity
4: colorIndexNumber
4: neededIndexNumber
4: color1
4: color2
4: color3
4: color4
4: color5
4: color6
4: color7
4: color8
4: color9
4: color10
4: color11
4: color12
4: color13
40: color1423
4: color24

|]

main = do
	[fn] <- getArgs
	cnt <- readFile fn
	print $ readBitmapFileHeader cnt

toRGB :: Int -> (Int, Int, Int)
toRGB rgb = let
	b = rgb .&. 0xff
	g = shiftR rgb 8 .&. 0xff
	r = shiftR rgb 16 in
	(r, g, b)
