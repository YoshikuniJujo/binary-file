{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary
import File.Binary.Instances.LittleEndian()
import File.Binary.Instances()
import System.Environment
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
	[inf] <- getArgs
	cnt <- readBinaryFile inf
	let (bmp, rest) = fromBinary () cnt :: (Bitmap, String)
	print bmp
	print $ colors bmp
	print rest

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	fromBinary _ s = let
		(b, rest) = fromBinary 1 s
		(g, rest') = fromBinary 1 rest
		(r, rest'') = fromBinary 1 rest' in
		((b, g, r), snd $ getBytes 1 rest'')
	toBinary _ (b, g, r) = concatBinary [
		toBinary 1 b,
		toBinary 1 g,
		toBinary 1 r,
		makeBinary $ BSL.singleton 0
	 ]

[binary|

Bitmap

2: "BM"
4: fileSize
2: 0
2: 0
4: offset

4: 40
4: width
4: height
2: 1
2: bits_per_pixel
4: compression
4: image_size
4: resolutionH
4: resolutionV
4: color_num
4: important_colors_num
((), Just color_num)<[(Int, Int, Int)]>: colors
((), Just image_size)<String>: image

|]
