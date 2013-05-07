{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import Control.Applicative
import BitmapCore
import Data.List
import Data.Maybe

main :: IO ()
main = do
	[inf, outf] <- getArgs
	Right (bmp :: Bitmap, "") <- fromBinary () <$> readBinaryFile inf
	putStrLn $ take 5000 (show bmp) ++ "..."
	let	img = bmpToImage bmp
		bmp' = imageToBMP img
		Right bin = toBinary () bmp'
	writeBinaryFile outf bin

type Image = [[RGB8]]
data RGB8 = RGB8 Int Int Int deriving (Show, Eq, Ord)

border, border2 :: Image
border = take 100 $ cycle
	[replicate 100 $ RGB8 0 0 0, replicate 100 $ RGB8 255 255 255]

border2 = take 100 $ cycle [
	replicate 100 $ RGB8 0 0 0, replicate 100 $ RGB8 255 255 255,
	replicate 100 $ RGB8 255 0 0, replicate 100 $ RGB8 0 0 255
 ]

rgb24ToRGB8 :: RGB24 -> RGB8
rgb24ToRGB8 (RGB24 r g b) = RGB8 r g b

rgb32ToRGB8 :: RGB32 -> RGB8
rgb32ToRGB8 (RGB32 r g b) = RGB8 r g b

rgb8ToRGB32 :: RGB8 -> RGB32
rgb8ToRGB32 (RGB8 r g b) = RGB32 r g b

rgb8ToRGB24 :: RGB8 -> RGB24
rgb8ToRGB24 (RGB8 r g b) = RGB24 r g b

linesToImage :: [RGB32] -> [Line] -> Image
linesToImage plt = reverse . map (pixelsToImage plt . line)

pixelsToImage :: [RGB32] -> Pixels -> [RGB8]
pixelsToImage _ (Colors24 cs) = map rgb24ToRGB8 cs
pixelsToImage _ (Colors32 cs) = map rgb32ToRGB8 cs
pixelsToImage plt (Indices is) = map rgb32ToRGB8 $ map (plt !!) is

bmpToImage :: Bitmap -> Image
bmpToImage = linesToImage <$> colors <*> image

getImage :: [RGB32] -> Image -> [Line]
getImage plt = map (Line . imageToPixels plt)

imageToPixels :: [RGB32] -> [RGB8] -> Pixels
imageToPixels plt ln
	| notBigger 256 plt =
		Indices $ map (fromJust . flip findIndex plt . (==) . rgb8ToRGB32) ln
	| otherwise = Colors24 $ map rgb8ToRGB24 ln

getMandrill :: IO Image
getMandrill = do
	Right (bmp, "") <- fromBinary () <$> readBinaryFile "tmp/Mandrill.bmp"
	return $ bmpToImage bmp

getLady :: IO Image
getLady = do
	Right (bmp, "") <- fromBinary () <$> readBinaryFile "tmp/test.bmp"
	return $ bmpToImage bmp

imageToBMP :: Image -> Bitmap
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

getColors :: Image -> [RGB32]
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

getISize :: [RGB32] -> Image -> Int
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

strsToImage :: [String] -> Image
strsToImage = map strToRGBs

strToRGBs :: String -> [RGB8]
strToRGBs = map $ fromJust . flip lookup [
	('*', RGB8 255 0 0),
	('#', RGB8 0 0 0),
	('.', RGB8 255 255 255)
 ]

strsToBin :: (Monad m, Applicative m) => [String] -> m String
strsToBin = toBinary () . imageToBMP . strsToImage

writeStrPic :: FilePath -> [String] -> IO ()
writeStrPic fp strs = writeBinaryFile fp =<< strsToBin strs

flipH = reverse
flipV = map reverse
scale img n = concatMap (replicate n . concatMap (replicate n)) img
above = (++)
beside = zipWith (++)

rotate = flipH . flipV

nega = map $ map $ \p -> case p of { '#' -> '.'; '*' -> '.'; '.' -> '*' }

superimpose = zipWith (zipWith combine)

combine topCh botCh
	| topCh == '#' = '#'
	| topCh == '*' = '*'
	| botCh == '#' = '#'
	| botCh == '*' = '*'
	| otherwise = '.'

sampleImg :: [String]
sampleImg = [
	"******************************************************",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*........*******************.........................*",
	"*.........*****************..........................*",
	"*..........***************...........................*",
	"*...........*************............................*",
	"*............***********.............................*",
	"*.............*********..............................*",
	"*..............*******...............................*",
	"*...............*****................................*",
	"*................***.................................*",
	"*.................*..................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**......................................*",
	"*............**....................***...............*",
	"*............**...................*****..............*",
	"*............**...................*****..............*",
	"*.................................*****..............*",
	"*..................................***...............*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"*....................................................*",
	"******************************************************"
 ]

horse :: [String]
horse = [
	".......##...",
	".....##..#..",
	"...##.....#.",
	"..#.......#.",
	"..#...#...#.",
	"..#...###.#.",
	".#....#..##.",
	"..#...#..##.",
	"...#...#....",
	"....#...#...",
	".....#..#...",
	"......#.#...",
	".......##..."
 ]

margeH :: [RGB8] -> [RGB8]
margeH [] = []
margeH [rgb] = [rgb]
margeH (RGB8 r1 g1 b1 : RGB8 r2 g2 b2 : rest) =
	RGB8 (average r1 r2) (average g1 g2) (average b1 b2) : margeH rest

average x y = (x + y) `div` 2
averageRGB8 (RGB8 r1 g1 b1) (RGB8 r2 g2 b2) =
	RGB8 (average r1 r2) (average g1 g2) (average b1 b2)

scaleDownH :: Image -> Image
scaleDownH = map margeH

scaleDownV :: Image -> Image
scaleDownV [] = []
scaleDownV [l] = [l]
scaleDownV (l1 : l2 : rest) = zipWith averageRGB8 l1 l2 : scaleDownV rest

scaleDown :: Image -> Image
scaleDown = scaleDownV . scaleDownH

getLena64 = scaleDown <$> scaleDown <$> scaleDown <$> getLady
getMandrill64 = scaleDown <$> scaleDown <$> scaleDown <$> getMandrill

writeImage :: FilePath -> Image -> IO ()
writeImage fp img = writeBinaryFile fp =<< toBinary () (imageToBMP img)

mirrorV img = img `beside` flipV img
mirrorH img = img `above` flipH img
mirror = mirrorV . mirrorH

scalePattern :: Image -> Image
scalePattern img =
	img' `beside` img' `above` img `beside` (img' `above` img' `above` img')
	where
	img' = scaleDown img
