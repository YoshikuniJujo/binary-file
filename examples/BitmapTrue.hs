module BitmapTrue (
	readBMPImage,
	writeBMPImage
) where

import BitmapCore
import Control.Applicative

readBMPImage :: FilePath -> IO Image
readBMPImage fp = fromBitmap . fst <$> (fromBinary () =<< readBinaryFile fp)

writeBMPImage :: FilePath -> Image -> IO ()
writeBMPImage fp img = writeBinaryFile fp =<< toBinary () (toBitmap img)

type Image = [[(Int, Int, Int)]]

fromBitmap :: Bitmap -> Image
fromBitmap Bitmap{
	fileSize = _fs,
	offset = _ofst,
	width = _w,
	height = _h,
	bits_per_pixel = 24,
	compression = 0,
	image_size = _is,
	resolutionH = _rh,
	resolutionV = _rv,
	color_num = _cn,
	important_colors_num = _icn,
	colors = [],
	image = ls
 } = map (pixelsToRGBs . line) ls
fromBitmap _ = error "BitmapTrue.fromBitmap"

pixelsToRGBs :: Pixels -> [(Int, Int, Int)]
pixelsToRGBs = map rgb24ToTuple . colors24

rgb24ToTuple :: RGB24 -> (Int, Int, Int)
rgb24ToTuple (RGB24 r g b) = (r, g, b)

toBitmap :: Image -> Bitmap
toBitmap img = Bitmap{
	fileSize = ofst + isize,
	offset = ofst,
	width = w,
	height = h,
	bits_per_pixel = 24,
	compression = 0,
	image_size = isize,
	resolutionH = 0,
	resolutionV = 0,
	color_num = 0,
	important_colors_num = 0,
	colors = [],
	image = reverse $ map (Line . listToPixels) img
 }	where
	w = length $ head img
	h = length img
	ofst = 54
	isize = padding (w * 3) * h

padding :: Int -> Int
padding x
	| x `mod` 4 == 0 = x
	| otherwise = x + 4 - x `mod` 4

listToPixels :: [(Int, Int, Int)] -> Pixels
listToPixels = Colors24 . map tupleToRGB24

tupleToRGB24 :: (Int, Int, Int) -> RGB24
tupleToRGB24 (r, g, b) = RGB24 r g b
