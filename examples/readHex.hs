{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary (binary, Field(..), readBinaryFile)
import File.Binary.Instances ()
import File.Binary.Instances.LittleEndian ()
import System.Environment (getArgs)
import Numeric (showHex)
import Data.List (intercalate)
import Control.Applicative ((<$>))

--------------------------------------------------------------------------------

main :: IO ()
main = do
	(h, "") <- fromBinary () <$> (readBinaryFile . head =<< getArgs)
	putStr $ unlines $ map (intercalate "-" . map unwords) $ groupN 2 $
		groupN 8 $ map (two . flip showHex "") $ hex h

two :: String -> String
two s = let l = length s in replicate (2 - l) '0' ++ s

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)
	
[binary|

Hex deriving Show

(1, Nothing){[Int]}: hex

|]
