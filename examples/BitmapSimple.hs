module BitmapSimple (
	Image(..),
	readBMPImage,
	writeBMPImage
) where

import BitmapCore
import Control.Applicative

readBMPImage :: FilePath -> IO Image
readBMPImage fp = fromBitmap . fst <$> (fromBinary () =<< readBinaryFile fp)

writeBMPImage :: FilePath -> Image -> IO ()
writeBMPImage fp img = writeBinaryFile fp =<< toBinary () (toBitmap img)

data Image = Image{
	pallet :: [(Int, Int, Int)],
	body :: [[Int]]
 } deriving Show

fromBitmap :: Bitmap -> Image
fromBitmap Bitmap{
	fileSize = _fs,
	offset = _ofst,
	width = _w,
	height = _h,
	bits_per_pixel = 8,
	compression = 0,
	image_size = _is,
	resolutionH = _rh,
	resolutionV = _rv,
	color_num = _cn,
	important_colors_num = _icn,
	colors = cs,
	image = ls
 } = Image{
	pallet = map rgb32ToTuple cs,
	body = reverse $ map (pixelsToList . line) ls
 }
fromBitmap _ = error "BitmapSimple.fromBitmap"

toBitmap :: Image -> Bitmap
toBitmap Image{
	pallet = p,
	body = b
 } = Bitmap{
	fileSize = ofst + isize,
	offset = ofst,
	width = w,
	height = h,
	bits_per_pixel = 8,
	compression = 0,
	image_size = isize,
	resolutionH = 0,
	resolutionV = 0,
	color_num = length p,
	important_colors_num = 0,
	colors = map tupleToRGB32 p,
	image = reverse $ map (Line . listToPixels) b
 }	where
	w = length $ head b
	h = length b
	ofst = length p * 4 + 54
	isize = padding w * h

padding :: Int -> Int
padding x
	| x `mod` 4 == 0 = x
	| otherwise = x + 4 - x `mod` 4

rgb32ToTuple :: RGB32 -> (Int, Int, Int)
rgb32ToTuple (RGB32 r g b) = (r, g, b)

tupleToRGB32 :: (Int, Int, Int) -> RGB32
tupleToRGB32 (r, g, b) = RGB32 r g b

pixelsToList :: Pixels -> [Int]
pixelsToList (Indices is) = is
pixelsToList _ = error "BitmapSimple.pixelsToList"

listToPixels :: [Int] -> Pixels
listToPixels is = Indices is
