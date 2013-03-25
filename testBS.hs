{-# LANGUAGE QuasiQuotes, FlexibleInstances #-}

import File.Binary
import System.Environment
import qualified Data.ByteString as BS
import Data.Int

main = do
	[inf] <- getArgs
	cnt <- BS.readFile inf

	let bmp = readBitmap cnt
	putStrLn $ (++ "...") $ take 4000 $ show $ readBitmap cnt

	let out = writeBitmap bmp {
		author_first = "Yoshikuni ",
		author_second = "Jujo      "
	 }

	BS.writeFile "tmp/out.bmp" out

tmpBMP = fmap readBitmap $ BS.readFile "tmp.bmp"

instance RetType Int16 where
	fromType n = fi n . fromIntegral
	toType = fromIntegral . ti

instance RetType (Int, Int, Int) where
	fromType n (b, g, r) = cc $ [
		fromType 1 b, fromType 1 g, fromType 1 r] ++ replicate (n - 3) zero
	toType str = let
		b = toType $ tk 1 str
		g = toType $ tk 1 $ dp 1 str
		r = toType $ tk 1 $ dp 2 str in
		(b, g, r)

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
2<Int16>: bitsPerPixel
4: compressionMethod
4: imageSize
4: horizontalResolution
4: verticalResolution
4: numberOfColors
4: importantColors
4<(Int, Int, Int)>[numberOfColors]: colors
-- 4[numberOfColors]: colors
imageSize<BS.ByteString>: image

10<String>: author_first
10<String>: author_second

|]
