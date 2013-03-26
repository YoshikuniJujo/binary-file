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
	readInt,
	isRepeat,
	getRepeat
) where

import Text.Peggy
import Here
import Control.Arrow
import Data.Char
import Language.Haskell.TH hiding (Type)
import Numeric hiding (readInt)

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

(4, colorIndexNumber)<[Int]>: colors
4<(Int,Int,Int)>[colorIndexNumber]: colors
-- 1[3]: image
imageSize<ByteString>: image
10<String>: author
10<ByteString>: hoge
10<Some>: some
10: "abc\n\r\SUB"
10: 0x89

repeat {

Chank

4: chankSize
4<String>: chankData
chankSize<String>: chankData
4<Word32>:chankCRC

}

|]

data Expression
	= Multiple Expression Expression
	| Division Expression Expression
	| Addition Expression Expression
	| Variable String
	| Number Int
	| ExpressionQ {expressionQ :: Name -> ExpQ}

instance Show Expression where
	show _ = "Expression"

sumExp :: [Expression] -> Expression
sumExp [] = Number 0
sumExp (e1 : e2) = Addition e1 $ sumExp e2

data ConstantValue
	= ConstantInt Int
	| ConstantString String
	deriving Show

constantInt endian (ConstantInt v) = v
constantInt endian (ConstantString v) = fromIntegral $ readInt endian v

data Type
--	= Tuple [Type]
	= Type { typeQ :: TypeQ } -- String

instance Show Type where
	show _ = "Type"

data VariableValue
	= VariableValue { variableValue :: String }
	deriving Show

data BinaryStructureItem
	= BinaryStructureItem {
		binaryStructureItemBytes :: Expression,
		binaryStructureItemType :: Type,
		binaryStructureItemListSize :: Maybe Expression, -- (Either Int String),
		binaryStructureItemValue :: Either ConstantValue VariableValue -- Int String
	 }
	| Repeat { getRepeat :: BinaryStructure }
	deriving Show

isRepeat (Repeat _) = True
isRepeat _ = False

bytesOf :: BinaryStructureItem -> Expression
bytesOf (Repeat BinaryStructure{binaryStructureBody = body}) =
	sumExp $ map bytesOf body
bytesOf BinaryStructureItem { binaryStructureItemBytes = b } = b

typeOf :: BinaryStructureItem -> Type
typeOf (Repeat BinaryStructure{binaryStructureName = name}) =
	Type $ appT listT $ conT $ mkName name
typeOf BinaryStructureItem{binaryStructureItemType = t} = t

sizeOf :: BinaryStructureItem -> Maybe Expression
sizeOf (Repeat BinaryStructure{}) = Nothing
sizeOf BinaryStructureItem{binaryStructureItemListSize = s} = s

valueOf :: Endian -> BinaryStructureItem -> Either Int String
valueOf endian BinaryStructureItem { binaryStructureItemValue = v } =
	(constantInt endian +++ variableValue) v
valueOf endian (Repeat BinaryStructure{binaryStructureName = name}) =
	Right $ "repeat" ++ name

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

readInt :: Endian -> String -> Integer
readInt LittleEndian "" = 0
readInt LittleEndian (c : cs) = fromIntegral (ord c) + 2 ^ 8 * readInt LittleEndian cs
readInt BigEndian str = readInt LittleEndian $ reverse str

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

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
	/ "repeat" spaces "{" top "}"
				{ Repeat $2 }
typ :: Type
	= [<] typeGen [>]	{ $2 }
	/ ""			{ Type $ conT $ mkName "Int" }

typeGen :: Type
	= [(] tupleGen_ [)]	{ Type $ tupT $2 }
	/ [\[] typeGen [\]]	{ Type $ appT listT $ typeQ $2 }
--	= [(] tupleGen [)]	{ Tuple $2 }
	/ [A-Z][.a-zA-Z0-9]*	{ Type $ conT $ mkName $ $1 : $2 }

typeGen_ :: TypeQ
	= [A-Z][.a-zA-Z0-9]*	{ conT $ mkName $ $1 : $2 }

tupleGen_ :: [TypeQ]
	= typeGen_ spaces "," spaces tupleGen_
				{ $1 : $4 }
	/ typeGen_ spaces "," spaces typeGen_
				{ [$1, $4] }

tupleGen :: [Type]
	= typeGen spaces "," spaces tupleGen
				{ $1 : $4 }
	/ typeGen spaces "," spaces typeGen
				{ [$1, $4] }

--	= [\(] [\)]		{ ExpressionQ $ const $ conE $ mkName "()" }
expr :: Expression
	= expr '*' expr		{ Multiple $1 $2 }
	/ expr '/' expr		{ Division $1 $2 }
	/ expr '+' expr		{ Addition $1 $2 }
	/ num			{ ExpressionQ $ const $ litE $ integerL $ fromIntegral $1 }
	/ var			{ ExpressionQ $ appE (varE $ mkName $1) . varE }
	/ [(] tupleExpr [)]	{ ExpressionQ $2 }
	/ 'Just' spaces expr	{ ExpressionQ $ \ret -> appE (conE $ mkName "Just") $
					expressionQ $2 ret }
	/ 'Nothing'		{ ExpressionQ $ const $ conE $ mkName "Nothing" }

--	/ [(] expr ', ' expr [)]
--				{ ExpressionQ $ \ret -> tupE
--					[expressionQ $2 ret, expressionQ $3 ret] }

tupleExpr :: Name -> ExpQ
	= expr ', ' expr	{ \ret -> tupE
					[expressionQ $1 ret, expressionQ $2 ret] }
	/ ""			{ const $ conE $ mkName "()" }

size :: Expression
	= '[' expr ']'

val :: Either ConstantValue VariableValue
	= num			{ Left $ ConstantInt $1 }
	/ var			{ Right $ VariableValue $1 }
	/ stringLit		{ Left $ ConstantString $1 }

stringLit :: String
	= '\"' strL '\"'

strL :: String
	= charLit*

charLit :: Char
	= [^\\\"]
	/ "\\" escLit

escLit :: Char
	= "n"			{ '\n' }
	/ "r"			{ '\r' }
	/ "\\"			{ '\\' }
	/ "SUB"			{ '\SUB' }

var :: String
	= [a-z][_a-zA-Z0-9]*	{ $1 : $2 }

num :: Int
	= '0x' [0-9a-fA-F]+	{ fst $ head $ readHex $1 }
	/ [1-9][0-9]*		{ read $ $1 : $2 }
	/ '0'			{ 0 }

|]
