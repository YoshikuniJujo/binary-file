{-# LANGUAGE TypeFamilies, PackageImports #-}

import Bitmap2
import "monads-tf" Control.Monad.Identity

import Control.Applicative

data RGB8 = RGB8 Int Int Int deriving Show
data Image = Image { imageX :: Int, imageY :: Int, imageBody :: [[RGB8]] }
	deriving Show

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
on3 :: (a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> b
on3 f p x y z = f (p x) (p y) (p z)
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

instance Color RGB8 where
	fromRGB8 = on3 RGB8 fi
	toRGB8 (RGB8 r g b) = on3 (,,) fi r g b

instance TwoDImage Image where
	type TwoDImageMonad Image = Identity
	type TwoDImageColor Image = RGB8
	type TwoDImageInput Image = Bitmap
	type TwoDImageOutput Image = Bitmap

	toTwoDImage =
		return . Image 0 0 . map (map $ uncurry3 fromRGB8) . bmpToImage
	fromTwoDImage = return . imageToBMP . map (map toRGB8) . imageBody

	makeTwoDImage w h getc = return $ Image 0 0 $ mkImage 0 0
		where
		mkImage x y
			| x < w = let
				(l : ls) = mkImage (x + 1) y in
				(getc x y : l) : ls
			| y < h - 1 = [] : mkImage 0 (y + 1)
			| otherwise = [[]]
	getTwoDImage Image{ imageBody = ib } = return
		((length $ head ib, length ib), getL 0 0 ib)
		where
		getL _ _ [[]] = []
		getL _ y ([] : css) = getL 0 (y + 1) css
		getL x y ((c : cs) : css) = ((x, y), c) : getL (x + 1) y (cs : css)
		getL _ _ _ = error "Main.TwoDImage getTwoDImage"

	up img@Image{ imageY = y } = return img{ imageY = y - 1 }
	down img@Image{ imageY = y } = return img{ imageY = y + 1 }
	left img@Image{ imageX = x } = return img{ imageX = x - 1 }
	right img@Image{ imageX = x } = return img{ imageX = x + 1 }

	getXY Image{ imageX = x, imageY = y } = return (x, y)
	setXY img x y = return img{ imageX = x, imageY = y }

readToImage :: FilePath -> IO Image
readToImage fp = runIdentity . toTwoDImage <$>
	(maybe (fail "bad") return =<< readBMP <$> readBinaryFile fp)

writeFromImage :: FilePath -> Image -> IO ()
writeFromImage fp img = maybe (fail "bad") (writeBinaryFile fp)
	(writeBMP $ runIdentity $ fromTwoDImage img)

getGradation, getSample, getLena, getMandrill :: IO Image
getGradation = readToImage "tmp/out/grad.bmp"
getSample = readToImage "tmp/out/sample.bmp"
getLena = readToImage "tmp/lena.bmp"
getMandrill = readToImage "tmp/Mandrill.bmp"

mapImage :: (RGB8 -> RGB8) -> Image -> Image
mapImage f img = img{ imageBody = map (map f) $ imageBody img }

brighten :: RGB8 -> RGB8
brighten (RGB8 r g b) = RGB8 (bright r) (bright g) (bright b)
	where
	bright w
		| w > 127 = 255
		| otherwise = 2 * w

brighten2 :: RGB8 -> RGB8
brighten2 (RGB8 r g b) = RGB8 (bright r) (bright g) (bright b)
	where
	bright w = 255 - (255 - w) `div` 2

darken :: RGB8 -> RGB8
darken (RGB8 r g b) = RGB8 (dark r) (dark g) (dark b)
	where
	dark w = w `div` 2

darken2 :: RGB8 -> RGB8
darken2 (RGB8 r g b) = RGB8 (dark r) (dark g) (dark b)
	where
	dark w	| w < 128 = 0
		| otherwise = 255 - (255 - w) * 2

nega :: RGB8 -> RGB8
nega (RGB8 r g b) = RGB8 (ng r) (ng g) (ng b)
	where
	ng w = 255 - w
