{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	BinaryStructure(..),
	BinaryStructureItem,
	bytesOf,
	sizeOf,
	valueOf,
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

4: 40
4: bitmapWidth
4: bitmapHeight
2: 1
2: bitPerPic
4: compress
4: imageDataSize
4: horizontalDensity
4: verticalDensity
4: colorIndexNumber
4: neededIndexNumber

4[colorIndexNumber]: colors
1[3]: image

|]

data BinaryStructureItem = BinaryStructureItem {
	binaryStructureItemBytes :: Int,
	binaryStructureItemListSize :: Maybe (Either Int String),
	binaryStructureItemValue :: Either Int String
 } deriving Show

bytesOf :: BinaryStructureItem -> Int
bytesOf = binaryStructureItemBytes

sizeOf :: BinaryStructureItem -> Maybe (Either Int String)
sizeOf = binaryStructureItemListSize

-- valueOf :: (Int, Either Int String) -> Either Int String
valueOf = binaryStructureItemValue

binaryStructureItem ::
	Int -> Maybe (Either Int String) -> Either Int String -> BinaryStructureItem
binaryStructureItem = BinaryStructureItem

{-
type BinaryStructureItem = (Int, Either Int String)

bytesOf :: (Int, Either Int String) -> Int
bytesOf = fst

valueOf :: (Int, Either Int String) -> Either Int String
valueOf = snd

binaryStructureItem :: Int -> Either Int String -> BinaryStructureItem
binaryStructureItem = (,)
-}

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureBody :: [BinaryStructureItem]
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

dat :: BinaryStructureItem
	= num size? ':' spaces val emptyLines	{ binaryStructureItem $1 $2 $4 }

size :: Either Int String
	= '[' val ']'		{ $1 }

val :: Either Int String
	= num			{ Left $1 }
	/ var			{ Right $1 }

var :: String
	= [a-z][a-zA-Z0-9]*	{ $1 : $2 }

num :: Int
	= [0-9]+		{ read $1 }

|]
