{-# LANGUAGE QuasiQuotes #-}

import Binary
import System.Environment
import qualified Data.ByteString as BS

main = do
	[inf] <- getArgs
	cnt <- BS.readFile inf

	let bmp = readBitmap cnt
	putStrLn $ (++ "...") $ take 4000 $ show $ readBitmap cnt

	let out = writeBitmap bmp {
		authorFirst = "Yoshikuni ",
		authorSecond = "Jujo      "
	 }

	BS.writeFile "tmp.bmp" out

tmpBMP = fmap readBitmap $ BS.readFile "tmp.bmp"

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
4<(Int,Int,Int)>[numberOfColors]: colors
-- 4[numberOfColors]: colors
imageSize<ByteString>: image

10<String>: authorFirst
10<String>: authorSecond

|]
