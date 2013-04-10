{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import File.Binary (binary, Field(..), Binary(..), readBinaryFile, writeBinaryFile)
import File.Binary.Instances.LittleEndian ()
import File.Binary.Instances ()
import Data.ByteString.Lazy (singleton)
import Data.Monoid (mconcat)
import Control.Applicative ((<$>))
import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[inf, outf] <- getArgs
	bmp <- readBitmap inf
	putStrLn $ take 1000 (show bmp) ++ "..."
	writeBitmap outf bmp

readBitmap :: FilePath -> IO Bitmap
readBitmap fp = do
	(bmp, "") <- fromBinary () <$> readBinaryFile fp
	return bmp

writeBitmap :: FilePath -> Bitmap -> IO ()
writeBitmap fp = writeBinaryFile fp . toBinary ()

instance Field (Int, Int, Int) where
	type FieldArgument (Int, Int, Int) = ()
	fromBinary _ s = let
		(b, rest) = fromBinary 1 s
		(g, rest') = fromBinary 1 rest
		(r, rest'') = fromBinary 1 rest' in
		((b, g, r), snd $ getBytes 1 rest'')
	toBinary _ (b, g, r) = mconcat [
		toBinary 1 b,
		toBinary 1 g,
		toBinary 1 r,
		makeBinary $ singleton 0
	 ]

[binary|

Bitmap

deriving Show

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
((), Just color_num){[(Int, Int, Int)]}: colors
((), Just image_size){String}: image

|]
