{-# LANGUAGE QuasiQuotes #-}

import QuoteBinaryStructure
import System.Environment
import Data.Bits
import System.IO

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
4[colorIndexNumber]: colors
4: image0

|]

-- 4[colorIndexNumber]: colors

main = do
	[fn] <- getArgs
	cnt <- readBinaryFile fn
--	print $ length cnt
	print $ readBitmapFileHeader cnt

toRGB :: Int -> (Int, Int, Int)
toRGB rgb = let
	b = rgb .&. 0xff
	g = shiftR rgb 8 .&. 0xff
	r = shiftR rgb 16 in
	(r, g, b)

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents
