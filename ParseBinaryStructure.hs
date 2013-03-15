{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	BinaryStructure(..),
	parseBinaryStructure
) where

import Text.Peggy
import Here

main :: IO ()
main = do
	putStrLn "ParseBinaryStructure"
	print $ parseBinaryStructure [here|

BitmapFileHeader

2: 19778
4: fileSize
2: 0
2: 0
4: offset

|]

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureBody :: [(Int, Either Int String)]
 } deriving Show

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

[peggy|

top :: BinaryStructure
	= emptyLines name emptyLines dat*
				{ BinaryStructure $2 $4 }

emptyLines :: ()
	= [ \n]*		{ () }

spaces :: ()
	= [ ]*			{ () }

name :: String
	= [A-Z][a-zA-Z0-9]*	{ $1 : $2 }

dat :: (Int, Either Int String)
	= num ':' spaces val '\n'	{ ($1, $3) }

val :: Either Int String
	= num			{ Left $1 }
	/ var			{ Right $1 }

var :: String
	= [a-z][a-zA-Z0-9]*	{ $1 : $2 }

num :: Int
	= [0-9]+		{ read $1 }

|]
