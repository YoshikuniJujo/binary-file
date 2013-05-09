{-# LANGUAGE QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BitmapCore (
	Bitmap(..),
	Line(..),
	Pixels(..),
	RGB24(..),
	RGB32(..),
	fromBinary,
	toBinary,
	consToBits,
	readBinaryFile,
	writeBinaryFile,

	paddBits
) where

import File.Binary (binary, Field(..), Binary(..), readBinaryFile, writeBinaryFile)
import File.Binary.Instances.LittleEndian ()
import File.Binary.Instances ()
import File.Binary.Instances.MSB0
import Data.ByteString.Lazy (singleton)
import Data.Monoid (mconcat)
import Control.Applicative ((<$>))
import Control.Arrow

--------------------------------------------------------------------------------

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
replicate color_num (){[RGB32]}: colors
replicate height (bits_per_pixel, width){[Line]}: image

|]

[binary|

Line deriving Show

arg :: (Int, Int)

(fst arg, snd arg){Pixels}: line
paddBits (fst arg * snd arg){BitsInt}: 0

|]

paddBits :: Int -> Int
paddBits n
	| n `mod` 32 == 0 = 0
	| otherwise = 32 - n `mod` 32

data Pixels
	= Indices [Int]
	| Colors24 { colors24 :: [RGB24] }
	| Colors32 [RGB32] deriving Show

instance Field Pixels where
	type FieldArgument Pixels = (Int, Int)
	fromBits (b, s) ([], bs)
		| b == 1 || b == 4 || b == 8 = first (Indices . map bitsInt) <$>
			fromBits (replicate s b) ([], bs)
		| b == 24 =
			first Colors24 <$> fromBits (replicate s ()) ([], bs)
		| b == 32 =
			first Colors32 <$> fromBits (replicate s ()) ([], bs)
		| otherwise = error "bad bits"
	fromBits _ _ = error
		"instance Field Pixels: fromBits _ (_ : _, _) is not implemented"
	consToBits (b, _) (Indices is) = consToBits (repeat b) (map BitsInt is)
	consToBits (_, _) (Colors24 rgbs) = consToBits (repeat ()) rgbs
	consToBits (_, _) (Colors32 rgbs) = consToBits (repeat ()) rgbs

data RGB24 = RGB24 Int Int Int deriving (Show, Eq)
data RGB32 = RGB32 Int Int Int deriving (Show, Eq)

instance Field RGB24 where
	type FieldArgument RGB24 = ()
	fromBinary _ s = do
		(b, rest) <- fromBinary 1 s
		(g, rest') <- fromBinary 1 rest
		(r, rest'') <- fromBinary 1 rest'
		return (RGB24 r g b, rest'')
	toBinary _ (RGB24 r g b) = do
		b' <- toBinary 1 b
		g' <- toBinary 1 g
		r' <- toBinary 1 r
		return $ mconcat [b', g', r']

instance Field RGB32 where
	type FieldArgument RGB32 = ()
	fromBinary _ s = do
		(b, rest) <- fromBinary 1 s
		(g, rest') <- fromBinary 1 rest
		(r, rest'') <- fromBinary 1 rest'
		return (RGB32 r g b, snd $ getBytes 1 rest'')
	toBinary _ (RGB32 r g b) = do
		b' <- toBinary 1 b
		g' <- toBinary 1 g
		r' <- toBinary 1 r
		return $ mconcat [b', g', r', makeBinary $ singleton 0]
