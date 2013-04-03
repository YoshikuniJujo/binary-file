{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, PackageImports #-}

{-# OPTIONS_GHC
	-fno-warn-unused-do-bind
	-fno-warn-unused-matches
	-fno-warn-name-shadowing #-}

module File.Binary.Parse (
	parse,
	BinaryStructure, bsName, bsArgName, bsArgType, bsBody,
	BinaryStructureItem, bytesOf, typeOf, valueOf,
	Value(..), variables,
	Expression, expression,
) where

import Prelude hiding (exp)
import Control.Applicative ((<$>), (<*>))
import "monads-tf" Control.Monad.Reader (Reader, runReader, ask)
import Numeric (readHex)

import Text.Peggy (peggy, parseString, space, defaultDelimiter)
import Language.Haskell.TH (
	ExpQ, litE, varE, appE, conE, tupE, integerL, infixApp,
	TypeQ, appT, conT, listT, tupleT, Name, mkName)

--------------------------------------------------------------------------------

parse :: String -> BinaryStructure
parse = either (error . show) id . parseString top ""

data BinaryStructure = BinaryStructure {
	bsName :: Name,
	bsArgName :: Name,
	bsArgType :: TypeQ,
	bsBody :: [BinaryStructureItem]
 }

data BinaryStructureItem = BinaryStructureItem {
	bytesOf :: Expression,
	typeOf :: TypeQ,
	valueOf :: Value
 }

type Expression	= Reader (ExpQ, ExpQ, Name) ExpQ

expression :: ExpQ -> ExpQ -> Name -> Expression -> ExpQ
expression ret arg argn e = runReader e (ret, arg, argn)

data Value
	= Constant { constant :: Either Integer String }
	| Variable { variable :: Name }

variables :: [Value] -> [Name]
variables =
	map variable . filter (\v -> case v of Variable _ -> True; _ -> False)

[peggy|

top :: BinaryStructure
	= empty lname arg dat*		{ BinaryStructure $2 (fst $3) (snd $3) $4 }

arg :: (Name, TypeQ)
	= empty var spaces '::' spaces typ
					{ ($2, $5) }
	/ ''				{ (mkName "_", conT $ mkName "()") }

dat :: BinaryStructureItem
	= empty exp spaces typeSpec spaces ':' spaces val
					{ BinaryStructureItem $2 $4 $7 }

typeSpec :: TypeQ
	= '{' typ '}'			{ $1 }
	/ ''				{ conT $ mkName "Int" }

val :: Value
	= var				{ Variable $1 }
	/ num				{ Constant $ Left $1 }
	/ string			{ Constant $ Right $1 }

exp :: Expression
	= exp spaces exp		{ appE <$> $1 <*> $3 }
	/ exp spaces op spaces exp	{ flip infixApp (varE $3) <$> $1 <*> $5 }
	/ '(' tupExp ')'
	/ '(' exp ')'
	/ '()'				{ return $ conE $ mkName "()" }
	/ num				{ return $ litE $ integerL $1 }
	/ lname				{ return $ conE $1 }
	/ var				{ flip fmap ask $ \(ret, arg, argn) ->
						if $1 == argn
							then arg
							else appE (varE $1) ret }
op :: Name
	= [!\\#$%&*+./<=>?@^|~-:]+	{ mkName $1 }
	/ '`' var '`'			{ $1 }
tupExp :: Expression = exp (spaces ',' spaces exp)+
	{ (.) tupE . (:) <$> $1 <*> mapM (\(_, _, e) -> e) $2 }

typ :: TypeQ
	= typ (spaces typ)+		{ foldl appT $1 $ map snd $2 }
	/ '(' tupType ')'		{ foldl appT (tupleT $ length $1) $1 }
	/ '(' typ ')'
	/ '()'				{ conT $ mkName "()" }
	/ '[' typ ']'			{ appT listT $1 }
	/ lname				{ conT $1 }
tupType :: [TypeQ]
	= typ spaces (',' spaces typ)+	{ $1 : map snd $3 }

lname :: Name
	= [A-Z][.a-zA-Z0-9_]*		{ mkName $ $1 : $2 }

var :: Name
	= [a-z][_a-zA-Z0-9]*		{ mkName $ $1 : $2 }

num :: Integer
	= '0x' [0-9a-fA-F]+		{ fst $ head $ readHex $1 }
	/ [1-9][0-9]*			{ read $ $1 : $2 }
	/ '0'				{ 0 }

string :: String = '\"' char* '\"'
char :: Char = [^\\\"] / '\\' esc
esc :: Char
	= 'n'				{ '\n' }
	/ 'r'				{ '\r' }
	/ '\\'				{ '\\' }
	/ 'SUB'				{ '\SUB' }

spaces :: () = (comm / [ \t])*				{ () }
empty :: () = (comm / lcomm / [ \n])*			{ () }
lcomm :: Char = '--' [^\n]* [\n]			{ ' ' }
comm :: Char = '{-' (!'{-' !'-}' . / comm)* '-}'	{ ' ' }

|]
