{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, TypeSynonymInstances,
	FlexibleInstances #-}

module ParseBinaryStructure (
	Endian(..),
	BinaryStructure(..),
	BinaryStructureItem,
	Expression(..),
	bytesOf,
	typeOf,
	sizeOf,
	valueOf,
	parseBinaryStructure,
	readInt,
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

type Expression	= Name -> ExpQ

multi, divi, addi :: Expression -> Expression -> Expression
multi = applyOp '(*)
divi = applyOp '(/)
addi = applyOp '(+)

applyOp :: Name -> Expression -> Expression -> Expression
applyOp op e1 e2 = \ret ->
	infixApp (e1 ret) (varE op) (e2 ret)

instance Show Expression where
	show _ = "Expression"

sumExp :: [Expression] -> Expression
sumExp [] = const $ litE $ integerL 0
sumExp (e1 : e2) = addi e1 $ sumExp e2

data ConstantValue
	= ConstantInt Int
	| ConstantString String
	deriving Show

constantInt endian (ConstantInt v) = v
constantInt endian (ConstantString v) = fromIntegral $ readInt endian v

instance Show TypeQ where
	show _ = "Type"

data VariableValue
	= VariableValue { variableValue :: String }
	deriving Show

data BinaryStructureItem
	= BinaryStructureItem {
		binaryStructureItemBytes :: Expression,
		binaryStructureItemType :: TypeQ,
		binaryStructureItemListSize :: Maybe Expression, -- (Either Int String),
		binaryStructureItemValue :: Either ConstantValue VariableValue -- Int String
	 }
	deriving Show

bytesOf :: BinaryStructureItem -> Expression
bytesOf BinaryStructureItem { binaryStructureItemBytes = b } = b

typeOf :: BinaryStructureItem -> TypeQ
typeOf BinaryStructureItem{binaryStructureItemType = t} = t

sizeOf :: BinaryStructureItem -> Maybe Expression
sizeOf BinaryStructureItem{binaryStructureItemListSize = s} = s

valueOf :: Endian -> BinaryStructureItem -> Either Int String
valueOf endian BinaryStructureItem { binaryStructureItemValue = v } =
	(constantInt endian +++ variableValue) v

binaryStructureItem :: Expression -> TypeQ -> Maybe Expression ->
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

typ :: TypeQ
	= [<] typeGen [>]	{ $2 }
	/ ""			{ conT $ mkName "Int" }

typeGen :: TypeQ
	= [(] tupleGen_ [)]	{ tupT $2 }
	/ [\[] typeGen [\]]	{ appT listT $ $2 }
	/ [A-Z][.a-zA-Z0-9]*	{ conT $ mkName $ $1 : $2 }

typeGen_ :: TypeQ
	= [A-Z][.a-zA-Z0-9]*	{ conT $ mkName $ $1 : $2 }

tupleGen_ :: [TypeQ]
	= typeGen_ spaces "," spaces tupleGen_
				{ $1 : $4 }
	/ typeGen_ spaces "," spaces typeGen_
				{ [$1, $4] }

tupleGen :: [TypeQ]
	= typeGen spaces "," spaces tupleGen
				{ $1 : $4 }
	/ typeGen spaces "," spaces typeGen
				{ [$1, $4] }

--	= [\(] [\)]		{ const $ conE $ mkName "()" }
expr :: Expression
	= expr '*' expr		{ multi $1 $2 }
	/ expr '/' expr		{ divi $1 $2 }
	/ expr '+' expr		{ addi $1 $2 }
	/ num			{ const $ litE $ integerL $ fromIntegral $1 }
	/ var			{ appE (varE $ mkName $1) . varE }
	/ [(] tupleExpr [)]	{ $2 }
	/ 'Just' spaces expr	{ \ret -> appE (conE $ mkName "Just") $
					$2 ret }
	/ 'Nothing'		{ const $ conE $ mkName "Nothing" }

--	/ [(] expr ', ' expr [)]
--				{ \ret -> tupE
--					[$2 ret, $3 ret] }

tupleExpr :: Name -> ExpQ
	= expr ', ' expr	{ \ret -> tupE
					[$1 ret, $2 ret] }
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
