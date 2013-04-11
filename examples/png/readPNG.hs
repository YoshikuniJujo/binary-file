{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import PNG

import System.Environment (getArgs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	[fin, fout] <- getArgs
	p <- readPNG fin
	putStrLn $ take 1000 (show p) ++ "..."

	let new = png (ihdr p) (bplte p) (plte p) (bidat p) (body p) (aplace p) (others p)

	writePNG fout new
	putStrLn ""
	putStrLn $ take 1000 (show new) ++ "..."

	putStrLn $ unwords $ map chunkName $ others p

{-
ihdr :: IHDR
ihdr = IHDR {
	width = 700,
	height = 700,
	depth = 8,
	alpha = False,
	color = True,
	palet = False,
	compressionType = 0,
	filterType = 0,
	interlaceType = 0 }
-}
