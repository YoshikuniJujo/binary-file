{-# LANGUAGE TypeFamilies #-}

import Bitmap
import Control.Applicative
import Control.Arrow

data RGB8 = RGB8 Int Int Int deriving (Show, Eq, Ord)
data Image = Image { imageX :: Int, imageY :: Int, getImg :: [[RGB8]] } deriving Show

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

instance Color RGB8 where
	fromRGB8 r g b = RGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
	toRGB8 (RGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

set :: [a] -> Int -> a -> [a]
set xs i x = take i xs ++ [x] ++ drop (i + 1) xs

getSample, getGradation :: IO Image
getSample = readBMPFile "tmp/out/sample.bmp"
getGradation = readBMPFile "tmp/out/grad.bmp"
