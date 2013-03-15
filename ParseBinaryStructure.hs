{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	BitmapStructure(..),
	parseBitmapStructure
) where

import Text.Peggy

main :: IO ()
main = do
	putStrLn "ParseBitmapStructure"

data BitmapStructure = BitmapStructure {
	bitmapStructureName :: String,
	bitmapStructureBody :: [(Int, Either Int String)]
 } deriving Show

parseBitmapStructure :: String -> BitmapStructure
parseBitmapStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

[peggy|

top :: BitmapStructure
	= spaces name	{ BitmapStructure $2 undefined }

spaces :: ()
	= [ \n]*		{ () }

name :: String
	= [A-Z][a-zA-Z0-9]*	{ $1 : $2 }

|]
