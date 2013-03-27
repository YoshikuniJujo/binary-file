{-# LANGUAGE QuasiQuotes, FlexibleInstances, TypeFamilies, OverloadedStrings #-}

import File.Binary
import File.Binary.Data.LittleEndian
import System.Environment
import qualified Data.ByteString as BS
import Data.Int
import Data.Word

main = do
	[inf] <- getArgs
	cnt <- BS.readFile inf

	let (bmp, rest) = readBitmap () $ cnt `BS.append` "  rest                "
	putStrLn $ (++ "...") $ take 4000 $ show $ bmp
	putStrLn $ (++ "...") $ show $ bmp
	print rest

	let out = writeBitmap bmp {
		author_first = "Yoshikuni ",
		author_second = "Jujo      "
	 }

	BS.writeFile "tmp/out.bmp" out

tmpBMP = fmap (readBitmap ()) $ BS.readFile "tmp.bmp"

instance RetType Int16 where
	type Argument Int16 = Int
	fromType n = fii n . fromIntegral
	toType n str = (fromIntegral $ fst $ tii n str, dp 2 str)

instance RetType Word8 where
	type Argument Word8 = Int
	fromType n = fii n . fromIntegral
	toType _ str = (fromIntegral $ ti $ tk 1 str, dp 1 str)

instance RetType (Word8, Word8, Word8) where
	type Argument (Word8, Word8, Word8) = Int
	fromType n (b, g, r) = cc $ [
		fromType 1 b, fromType 1 g, fromType 1 r, zero]
	toType n str = let
		(b, rest) = toType 1 str
		(g, rest') = toType 1 rest
		(r, rest'') = toType 1 rest' in
		((b, g, r), dp 1 rest'')

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
(4, Just numberOfColors)<[(Word8, Word8, Word8)]>: colors
imageSize<BS.ByteString>: image

((), Just 10)<String>: author_first
((), Just 10)<String>: author_second
-- ((), Nothing)<String>: rest

|]
