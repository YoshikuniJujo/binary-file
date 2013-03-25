{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	Endian(..),
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

set big_endian

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
10<Some>: some

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

constantInt endian (ConstantInt v) = v
constantInt endian (ConstantString v) = readInt endian v

data Type
	= String | Int | ByteString | Tuple [Type]
	| Type String
	deriving (Show, Eq)

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

valueOf :: Endian -> BinaryStructureItem -> Either Int String
valueOf endian = (constantInt endian +++ variableValue) . binaryStructureItemValue

binaryStructureItem :: Expression -> Type -> Maybe Expression ->
	Either ConstantValue VariableValue -> BinaryStructureItem
binaryStructureItem = BinaryStructureItem

data Endian = BigEndian | LittleEndian deriving Show

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureEndian :: Endian,
	binaryStructureBody :: [BinaryStructureItem]
 } deriving Show

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

readInt :: Endian -> String -> Int
readInt LittleEndian "" = 0
readInt LittleEndian (c : cs) = ord c + 2 ^ 8 * readInt LittleEndian cs
readInt BigEndian str = readInt LittleEndian $ reverse str

[peggy|

top :: BinaryStructure
	= emptyLines name emptyLines endian emptyLines dat*
				{ BinaryStructure $2 $4 $6 }
	/ emptyLines name emptyLines dat*
				{ BinaryStructure $2 LittleEndian $4 }

endian :: Endian
	= "set" spaces "big_endian"
				{ BigEndian }

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
	/ [A-Z][a-zA-Z0-9]*	{ Type $ $1 : $2 }

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
	= [a-z][_a-zA-Z0-9]*	{ $1 : $2 }

num :: Int
	= [0-9]+		{ read $1 }

|]
