{-# LANGUAGE QuasiQuotes #-}

import Binary
import System.Environment

main = do
	[inf, outf] <- getArgs

	cnt <- readBinaryFile inf
	let bmp = readBitmap cnt
	print $ readBitmap cnt

	let out = writeBitmap bmp {
		authorFirst = "Yoshikuni ",
		authorSecond = "Jujo      "
	 }
	writeBinaryFile outf out

[binary|

Bitmap

2: "BM"
4: fileSize
2: 0
2: 0
4: offset
4: 40
4: bitmapWidth
4: bitmapHeight
2: 1
2: bitsPerPixel
4: compressionMethod
4: imageSize
4: horizontalResolution
4: verticalResolution
4: numberOfColors
4: importantColors
4[numberOfColors]: colors
bitsPerPixel/8[imageSize*8/bitsPerPixel]: image
10<String>: authorFirst
10<String>: authorSecond

|]
