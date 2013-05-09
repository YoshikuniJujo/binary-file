{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module Bitmap (
	bmpToTwoDImage,
	twoDImageToBMP,
	TwoDImage(..),
	Color(..),
	readBMPFile
) where

import BitmapCore
import Data.Image
import Control.Applicative
import Control.Arrow

-- import Data.Word
import Data.List
import Data.Maybe

import File.Binary

data RGB8 = RGB8 Int Int Int deriving (Show, Eq, Ord)
data Image = Image { imageX :: Int, imageY :: Int, getImg :: [[RGB8]] }

instance TwoDImage Image where
	type TwoDImageColor Image = RGB8
	new c w h = return $ Image 0 0 $ replicate h $ replicate w c
	fromColorList = return . Image 0 0
	toColorList = return . getImg
	getSize = return . (length . head &&& length) . getImg
	getXY = return . (imageX &&& imageY)
	setXY img (x, y) = return img { imageX = x, imageY = y }
	getPixel = return . ((!!) <$> ((!!) <$> getImg <*> imageY) <*> imageX)
	setPixel img c = return $ img { getImg = getImg img `set` imageY img $
		(getImg img !! imageY img) `set` imageX img $ c }

readBMPFile :: (TwoDImage i, Color (TwoDImageColor i)) => FilePath -> IO i
readBMPFile = (bmpToTwoDImage =<<) . readBinaryFile

bmpToTwoDImage :: (Binary b, TwoDImage i, Color (TwoDImageColor i),
	Monad m, Applicative m) => b -> m i
bmpToTwoDImage b = do
	(bmp :: Bitmap, _) <- fromBinary () b
	fromColorList $ bmpToColorList bmp

twoDImageToBMP :: (Binary b, TwoDImage i, Color (TwoDImageColor i),
	Monad m, Applicative m) => i -> m b
twoDImageToBMP i = do
	bmp :: Bitmap <- colorListToBMP <$> toColorList i
	toBinary () bmp

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 f p x y z = f (p x) (p y) (p z)

bmpToColorList :: Color c => Bitmap -> [[c]]
bmpToColorList =
	map (map (\(RGB8 r g b) -> fromRGB8 (fi r) (fi g) (fi b))) . bmpToImage

colorListToBMP :: Color c => [[c]] -> Bitmap
colorListToBMP = imageToBMP . map (map (uncurry3 (on3 RGB8 fi) . toRGB8))

bmpToImage :: Bitmap -> [[RGB8]]
bmpToImage = linesToImage <$> colors <*> image

imageToBMP :: [[RGB8]] -> Bitmap
imageToBMP img_ = Bitmap {
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

set :: [a] -> Int -> a -> [a]
set xs i x = take i xs ++ [x] ++ drop (i + 1) xs

{-
getSample, getGradation :: IO Bitmap
getSample = fst <$> (fromBinary () =<< readBinaryFile "sample.bmp")
getGradation = fst <$> (fromBinary () =<< readBinaryFile "grad.bmp")
-}

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
notBigger n (x : xs) = notBigger (n - 1) xs

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
