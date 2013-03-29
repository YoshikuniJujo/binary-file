{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes,
	FlexibleContexts,
	FlexibleInstances,
	PackageImports #-}

{-# OPTIONS_GHC
	-fno-warn-unused-do-bind
	-fno-warn-unused-matches
	-fno-warn-name-shadowing
	-fno-warn-orphans #-}

module ParseBinaryStructure (
	parseBinaryStructure,

	BinaryStructure,
	binaryStructureName,
	binaryStructureArgName,
	binaryStructureArgType,
	binaryStructureBody,

	BinaryStructureItem,
	bytesOf,
	typeOf,
	valueOf,

	Expression,
	expression,
	Binary(..),
	Field(..),
	fii, tii,
	dp, fs, cc
) where

import Text.Peggy
import Language.Haskell.TH
import Numeric

import Classes

import "monads-tf" Control.Monad.Reader
import Control.Applicative

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureArgName :: String,
	binaryStructureArgType :: TypeQ,
	binaryStructureBody :: [BinaryStructureItem]
 }

data BinaryStructureItem = BinaryStructureItem {
	bytesOf :: Expression,
	typeOf :: TypeQ,
	valueOf :: Either (Either Int String) String
 }

type Expression	= Reader (Name, Name, String) ExpQ

expression :: Name -> Name -> String -> Expression -> ExpQ
expression ret arg argn e = runReader e (ret, arg, argn)

applyOp :: Name -> Expression -> Expression -> Expression
applyOp op re1 re2 = flip infixApp (varE op) <$> re1 <*> re2

[peggy|

top :: BinaryStructure
	= emptyLines name emptyLines argType dat*
				{ BinaryStructure $2 (fst $4) (snd $4) $5 }

argType :: (String, TypeQ)
	= spaces var spaces '::' spaces typeGen [\n]+
				{ ($2, $5) }
	/ ""			{ ("_", conT $ mkName "()") }

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
	/ num			{ return $ litE $ integerL $ fromIntegral $1 }
	/ var			{ ask >>= \(ret, arg, argn) -> return $
					if $1 == argn
						then varE arg
						else appE (varE $ mkName $1) $
							varE ret }
	/ [(] tupleExpr [)]	{ $2 }
	/ 'Just' spaces expr	{ appE (conE $ mkName "Just") <$> $2 }
	/ 'Nothing'		{ return $ conE $ mkName "Nothing" }

tupleExpr :: Expression
	= expr ', ' expr	{ (\e1 e2 -> tupE [e1, e2]) <$> $1 <*> $2 }
	/ expr
	/ ""			{ return $ conE $ mkName "()" }

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
