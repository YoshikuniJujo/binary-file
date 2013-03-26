{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_GHC
	-fno-warn-unused-do-bind
	-fno-warn-unused-matches
	-fno-warn-name-shadowing
	-fno-warn-orphans #-}

module ParseBinaryStructure (
	Endian(..),
	BinaryStructure(..),
	BinaryStructureItem,
	Expression,
	bytesOf,
	typeOf,
	sizeOf,
	valueOf,
	parseBinaryStructure,
	readInt,
	Str(..),
	RetType(..),
	fii, fiiBE,
	tii, tiiBE
) where

import Text.Peggy
import Control.Arrow
import Language.Haskell.TH hiding (Type)
import Numeric hiding (readInt)

import Classes

type Expression	= Name -> ExpQ

multi, divi, addi :: Expression -> Expression -> Expression
multi = applyOp '(*)
divi = applyOp '(/)
addi = applyOp '(+)

applyOp :: Name -> Expression -> Expression -> Expression
applyOp op e1 e2 ret = infixApp (e1 ret) (varE op) (e2 ret)

instance Show Expression where
	show _ = "Expression"

data ConstantValue
	= ConstantInt Int
	| ConstantString String
	deriving Show

constantInt :: Endian -> ConstantValue -> Int
constantInt _ (ConstantInt v) = v
constantInt end (ConstantString v) = fromIntegral $ readInt end v

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
valueOf end BinaryStructureItem { binaryStructureItemValue = v } =
	(constantInt end +++ variableValue) v

binaryStructureItem :: Expression -> TypeQ -> Maybe Expression ->
	Either ConstantValue VariableValue -> BinaryStructureItem
binaryStructureItem = BinaryStructureItem

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureEndian :: Endian,
	binaryStructureBody :: [BinaryStructureItem]
 } deriving Show

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

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
	= [(] tupleGen_ [)]	{ foldl appT (tupleT $ length $2) $2 }
	/ [\[] typeGen [\]]	{ appT listT $ $2 }
	/ [A-Z][.a-zA-Z0-9]*	{ conT $ mkName $ $1 : $2 }

typeGen_ :: TypeQ
	= [A-Z][.a-zA-Z0-9]*	{ conT $ mkName $ $1 : $2 }

tupleGen_ :: [TypeQ]
	= typeGen_ spaces "," spaces tupleGen_
				{ $1 : $4 }
	/ typeGen_ spaces "," spaces typeGen_
				{ [$1, $4] }

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
