{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.LittleEndian
import System.Environment
import Control.Applicative
import Numeric

main = do
	cnt <- readBinaryFile . head =<< getArgs
	putStrLn $ unlines $ map unwords $ groupN 16 $ map (two . flip showHex "") $
		hex $ fst (fromBinary () cnt :: (Hex, String))

two s = replicate (2 - l) '0' ++ s
	where
	l = length s

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)
	
[binary|

Hex deriving Show

(1, Nothing){[Int]}: hex

|]
