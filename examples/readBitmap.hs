{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Control.Applicative
import BitmapCore
import Data.List

main :: IO ()
main = do
	[inf, outf] <- getArgs
	Right (bmp :: Bitmap, "") <- fromBinary () <$> readBinaryFile inf
	putStrLn $ take 5000 (show bmp) ++ "..."
	let Right bin = toBinary () bmp
	writeBinaryFile outf bin

type Image = [[RGB8]]
data RGB8 = RGB8 Int Int Int deriving (Show, Eq, Ord)

rgb24ToRGB8 :: RGB24 -> RGB8
rgb24ToRGB8 (RGB24 r g b) = RGB8 r g b

rgb32ToRGB8 :: RGB32 -> RGB8
rgb32ToRGB8 (RGB32 r g b) = RGB8 r g b

rgb8ToRGB32 :: RGB8 -> RGB32
rgb8ToRGB32 (RGB8 r g b) = RGB32 r g b

linesToImage :: [RGB32] -> [Line] -> Image
linesToImage plt = map (pixelsToImage plt . line)

pixelsToImage :: [RGB32] -> Pixels -> [RGB8]
pixelsToImage _ (Colors24 cs) = map rgb24ToRGB8 cs
pixelsToImage _ (Colors32 cs) = map rgb32ToRGB8 cs
pixelsToImage plt (Indices is) = map rgb32ToRGB8 $ map (plt !!) is

bmpToImage :: Bitmap -> Image
bmpToImage = linesToImage <$> colors <*> image

getMandrill :: IO Image
getMandrill = do
	Right (bmp, "") <- fromBinary () <$> readBinaryFile "tmp/Mandrill.bmp"
	return $ bmpToImage bmp

getLady :: IO Image
getLady = do
	Right (bmp, "") <- fromBinary () <$> readBinaryFile "tmp/test.bmp"
	return $ bmpToImage bmp

-- imageToBMP :: Image -> Bitmap

getColors :: Image -> [RGB32]
getColors = map (rgb8ToRGB32 . head) . group . sort . concat

notBigger :: Int -> [a] -> Bool
notBigger _ [] = True
notBigger 0 _ = False
notBigger n (x : xs) = notBigger (n - 1) xs
