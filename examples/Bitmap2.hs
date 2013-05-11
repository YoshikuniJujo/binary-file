{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module Bitmap2 (
	TwoDImage(..),
	Color(..),
	Bitmap(..),
	bmpToImage,
	imageToBMP,
	readBMP,
	writeBMP,
	readBinaryFile,
	writeBinaryFile
) where

import BitmapCore
import Data.Image2
import Control.Applicative
-- import Control.Arrow

import Data.Word
import Data.List
import Data.Maybe

import File.Binary

data RGB8 = RGB8 Int Int Int deriving (Show, Eq, Ord)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

on3 :: (a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> b
on3 f p x y z = f (p x) (p y) (p z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

rgb8ToTuple :: RGB8 -> (Word8, Word8, Word8)
rgb8ToTuple (RGB8 r g b) = on3 (,,) fi r g b

tupleToRGB8 :: (Word8, Word8, Word8) -> RGB8
tupleToRGB8 = uncurry3 $ on3 RGB8 fi

readBMP :: Binary b => b -> Maybe Bitmap
readBMP = fmap fst . fromBinary ()

writeBMP :: Binary b => Bitmap -> Maybe b
writeBMP = toBinary ()

bmpToImage :: Bitmap -> [[(Word8, Word8, Word8)]]
bmpToImage = map (map rgb8ToTuple) . (linesToImage <$> colors <*> image)

imageToBMP :: [[(Word8, Word8, Word8)]] -> Bitmap
imageToBMP = imageToBMP_ . map (map tupleToRGB8)

imageToBMP_ :: [[RGB8]] -> Bitmap
imageToBMP_ img_ = Bitmap {
		fileSize = ofst + isize,
		offset = ofst,
		width = length $ head img,
		height = length img,
		bits_per_pixel = getbpp plt,
		compression = 0,
		image_size = getISize plt img,
		resolutionH = 0,
		resolutionV = 0,
		color_num = cnum,
		important_colors_num = 0,
		colors = if notBigger 256 plt then plt else [],
		image = getImage plt img
	 }
	where
	plt = getColors img
	cnum = if notBigger 256 plt then length plt else 0
	ofst = getOffset plt
	isize = getISize plt img
	img = reverse img_

rgb24ToRGB8 :: RGB24 -> RGB8
rgb24ToRGB8 (RGB24 r g b) = RGB8 r g b

rgb32ToRGB8 :: RGB32 -> RGB8
rgb32ToRGB8 (RGB32 r g b) = RGB8 r g b

rgb8ToRGB32 :: RGB8 -> RGB32
rgb8ToRGB32 (RGB8 r g b) = RGB32 r g b

rgb8ToRGB24 :: RGB8 -> RGB24
rgb8ToRGB24 (RGB8 r g b) = RGB24 r g b

linesToImage :: [RGB32] -> [Line] -> [[RGB8]]
linesToImage plt = reverse . map (pixelsToImage plt . line)

pixelsToImage :: [RGB32] -> Pixels -> [RGB8]
pixelsToImage _ (Colors24 cs) = map rgb24ToRGB8 cs
pixelsToImage _ (Colors32 cs) = map rgb32ToRGB8 cs
pixelsToImage plt (Indices is) = map rgb32ToRGB8 $ map (plt !!) is

getColors :: [[RGB8]] -> [RGB32]
getColors = map (rgb8ToRGB32 . head) . group . sort . concat

notBigger :: Int -> [a] -> Bool
notBigger _ [] = True
notBigger 0 _ = False
notBigger n (_ : xs) = notBigger (n - 1) xs

getbpp :: [RGB32] -> Int
getbpp cs
	| notBigger 2 cs = 1
	| notBigger 16 cs = 4
	| notBigger 256 cs = 8
	| otherwise = 24

getISize :: [RGB32] -> [[RGB8]] -> Int
getISize plt img = addPadd (bpp * w) `div` 8 * h
	where
	bpp = getbpp plt
	w = length $ head img
	h = length img
	addPadd x = x + paddBits x

getOffset :: [RGB32] -> Int
getOffset plt
	| notBigger 256 plt = length plt * 4 + 54
	| otherwise = 54

getImage :: [RGB32] -> [[RGB8]] -> [Line]
getImage plt = map (Line . imageToPixels plt)

imageToPixels :: [RGB32] -> [RGB8] -> Pixels
imageToPixels plt ln
	| notBigger 256 plt =
		Indices $ map (fromJust . flip findIndex plt . (==) . rgb8ToRGB32) ln
	| otherwise = Colors24 $ map rgb8ToRGB24 ln
