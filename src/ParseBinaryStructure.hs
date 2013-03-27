{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_GHC
	-fno-warn-unused-do-bind
	-fno-warn-unused-matches
	-fno-warn-name-shadowing
	-fno-warn-orphans #-}

module ParseBinaryStructure (
	parseBinaryStructure,

	BinaryStructure,
	binaryStructureName,
	binaryStructureArgType,
	binaryStructureBody,

	BinaryStructureItem,
	bytesOf,
	typeOf,
	valueOf,

	Expression,
	Str(..),
	RetType(..),
	fii, fiiBE,
	tii, tiiBE
) where

import Text.Peggy
import Language.Haskell.TH
import Numeric

import Classes

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureArgType :: TypeQ,
	binaryStructureBody :: [BinaryStructureItem]
 }

data BinaryStructureItem = BinaryStructureItem {
	bytesOf :: Expression,
	typeOf :: TypeQ,
	valueOf :: Either (Either Int String) String
 }

type Expression	= Name -> ExpQ

applyOp :: Name -> Expression -> Expression -> Expression
applyOp op e1 e2 ret = infixApp (e1 ret) (varE op) (e2 ret)

[peggy|

top :: BinaryStructure
	= emptyLines name emptyLines argType dat*
				{ BinaryStructure $2 $4 $5 }

argType :: TypeQ
	= typ [\n]+		{ $1 }
	/ ""			{ conT $ mkName "()" }

emptyLines :: ()
	= "--" [^\n]* [\n]	{ () }
	/ [ \n]*		{ () }

spaces :: ()
	= [ ]*			{ () }


name :: String
	= [A-Z][a-zA-Z0-9]*	{ $1 : $2 }

dat :: BinaryStructureItem
	= expr typ size? ':' spaces val emptyLines
				{ BinaryStructureItem $1 $2 $5 }

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
	= expr spaces '*' spaces expr		{ applyOp (mkName "*") $1 $4 }
	/ expr spaces '`div`' spaces expr	{ applyOp (mkName "div") $1 $4 }
	/ expr spaces '+' spaces expr		{ applyOp (mkName "+") $1 $4 }
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

val :: Either (Either Int String) String
	= num			{ Left $ Left $1 }
	/ var			{ Right $ $1 }
	/ stringLit		{ Left $ Right $1 }

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
