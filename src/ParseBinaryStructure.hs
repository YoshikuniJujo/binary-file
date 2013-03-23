{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	BinaryStructure(..),
	BinaryStructureItem,
	Expression(..),
	Type(..),
	bytesOf,
	typeOf,
	sizeOf,
	valueOf,
	parseBinaryStructure,
	readInt
) where

import Text.Peggy
import Here
import Control.Arrow
import Data.Char

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

4<(Int,Int,Int)>[colorIndexNumber]: colors
-- 1[3]: image
imageSize<ByteString>: image
10<String>: author
10<ByteString>: hoge

|]

data Expression
	= Multiple Expression Expression
	| Division Expression Expression
	| Variable String
	| Number Int
	deriving Show

data ConstantValue
	= ConstantInt Int
	| ConstantString String
	deriving Show

constantInt (ConstantInt v) = v
constantInt (ConstantString v) = readInt v

data Type = String | Int | ByteString | Tuple [Type] deriving (Show, Eq)

data VariableValue
	= VariableValue { variableValue :: String }
	deriving Show

data BinaryStructureItem = BinaryStructureItem {
	binaryStructureItemBytes :: Expression,
	binaryStructureItemType :: Type,
	binaryStructureItemListSize :: Maybe Expression, -- (Either Int String),
	binaryStructureItemValue :: Either ConstantValue VariableValue -- Int String
 } deriving Show

bytesOf :: BinaryStructureItem -> Expression
bytesOf = binaryStructureItemBytes

typeOf :: BinaryStructureItem -> Type
typeOf = binaryStructureItemType

sizeOf :: BinaryStructureItem -> Maybe Expression
sizeOf = binaryStructureItemListSize

valueOf :: BinaryStructureItem -> Either Int String
valueOf = (constantInt +++ variableValue) . binaryStructureItemValue

binaryStructureItem :: Expression -> Type -> Maybe Expression ->
	Either ConstantValue VariableValue -> BinaryStructureItem
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

readInt :: String -> Int
readInt "" = 0
readInt (c : cs) = ord c + 2 ^ 8 * readInt cs

[peggy|

top :: BinaryStructure
	= emptyLines name emptyLines dat*
				{ BinaryStructure $2 $4 }

emptyLines :: ()
	= "--" [^\n]* [\n]	{ () }
	/ [ \n]*		{ () }

spaces :: ()
	= [ ]*			{ () }


name :: String
	= [A-Z][a-zA-Z0-9]*	{ $1 : $2 }

dat :: BinaryStructureItem
	= expr typ size? ':' spaces val emptyLines
				{ binaryStructureItem $1 $2 $3 $5 }
typ :: Type
	= [<] typeGen [>]	{ $2 }
	/ ""			{ Int }

typeGen :: Type
	= [(] tupleGen [)]	{ Tuple $2 }
	/ "String"		{ String }
	/ "ByteString"		{ ByteString }
	/ "Int"			{ Int }

tupleGen :: [Type]
	= typeGen spaces "," spaces tupleGen
				{ $1 : $4 }
	/ typeGen spaces "," spaces typeGen
				{ [$1, $4] }

expr :: Expression
	= expr '*' expr		{ Multiple $1 $2 }
	/ expr '/' expr		{ Division $1 $2 }
	/ num			{ Number $1 }
	/ var			{ Variable $1 }

size :: Expression
	= '[' expr ']'

val :: Either ConstantValue VariableValue
	= num			{ Left $ ConstantInt $1 }
	/ var			{ Right $ VariableValue $1 }
	/ stringL		{ Left $ ConstantString $1 }

stringL :: String
	= '\"' [^\"]* '\"'

var :: String
	= [a-z][a-zA-Z0-9]*	{ $1 : $2 }

num :: Int
	= [0-9]+		{ read $1 }

|]
