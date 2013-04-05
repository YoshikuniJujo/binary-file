{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, PackageImports #-}

{-# OPTIONS_GHC
	-fno-warn-name-shadowing
	-fno-warn-unused-binds
	-fno-warn-unused-matches
	-fno-warn-unused-do-bind #-}

module File.Binary.Parse (
	parse,
	BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsItem,
	BSItem, bytesOf, valueOf,
	Value(..), variables,
	Expression, expression,
) where

import Prelude hiding (exp)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import "monads-tf" Control.Monad.Reader (Reader, runReader, ask)
import Numeric (readHex)

import Text.Peggy (peggy, parseString, space, defaultDelimiter)
import Language.Haskell.TH (
	ExpQ, litE, varE, conE, appE, tupE, integerL, uInfixE, parensE,
	TypeQ, appT, conT, listT, tupleT, Name, mkName)

--------------------------------------------------------------------------------

parse :: String -> BinaryStructure
parse = either (error . show) id . parseString top ""

data BinaryStructure = BinaryStructure {
	bsName :: Name,
	bsDerive :: [Name],
	bsArgName :: Name,
	bsArgType :: TypeQ,
	bsItem :: [BSItem]
 }

data BSItem = BSItem {
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

variables :: [BSItem] -> [(Name, TypeQ)]
variables = map (variable . valueOf &&& typeOf) .
	filter (\bsi -> case valueOf bsi of Variable _ -> True; _ -> False)

{-
variables :: [Value] -> [Name]
variables =
	map variable . filter (\v -> case v of Variable _ -> True; _ -> False)
-}

[peggy|

top :: BinaryStructure
	= emp lname der arg dat*
				{ BinaryStructure $2 $3 (fst $4) (snd $4) $5 }

der :: [Name]
	= emp 'deriving' sp ln (sp ',' sp ln)*
				{ mkName $3 : map (\(_, _, n) -> mkName n) $4 }
	/ ''			{ [] }

arg :: (Name, TypeQ)
	= emp var sp '::' sp typ	{ ($2, $5) }
	/ ''				{ (mkName "_", conT $ mkName "()") }

dat :: BSItem
	= emp exp sp typS sp ':' sp val	{ BSItem $2 $4 $7 }

typS :: TypeQ
	= '{' typ '}'			{ $1 }
	/ ''				{ conT $ mkName "Int" }

val :: Value
	= var				{ Variable $1 }
	/ num				{ Constant $ Left $1 }
	/ string			{ Constant $ Right $1 }

exp :: Expression
	= exp sp op sp expOp1		{ uInfixE <$> $1 <*> $3 <*> $5 }
	/ expOp1

expOp1 :: Expression
	= expOp1 sp exp1		{ appE <$> $1 <*> $3 }
	/ exp1

exp1 :: Expression
	= '(' tupExp ')'
	/ '(' exp ')'			{ parensE <$> $1 }
	/ '()'				{ return $ conE $ mkName "()" }
	/ num				{ return $ litE $ integerL $1 }
	/ lname				{ return $ conE $1 }
	/ var				{ flip fmap ask $ \(ret, arg, argn) ->
						if $1 == argn
							then arg
							else appE (varE $1) ret }
op :: Expression
	= [!\\#$%&*+./<=>?@^|~-:]+	{ return $ varE $ mkName $1 }
	/ '`' var '`'			{ return $ varE $1 }
tupExp :: Expression = exp (sp ',' sp exp)+
	{ (.) tupE . (:) <$> $1 <*> mapM (\(_, _, e) -> e) $2 }

typ :: TypeQ
	= typ typ1			{ appT $1 $2 }
	/ typ1

typ1 :: TypeQ
	= '(' tupType ')'		{ foldl appT (tupleT $ length $1) $1 }
	/ '(' typ ')'
	/ '()'				{ conT $ mkName "()" }
	/ '[' typ ']'			{ appT listT $1 }
	/ lname				{ conT $1 }
tupType :: [TypeQ]
	= typ sp (',' sp typ)+	{ $1 : map snd $3 }

lname :: Name
	= (ln '.')* ln			{ mkName $ concatMap (++ ".") $1 ++ $2 }

var :: Name
	= (ln '.')* sn			{ mkName $ concatMap (++ ".") $1 ++ $2 }

num :: Integer
	= '0x' [0-9a-fA-F]+		{ fst $ head $ readHex $1 }
	/ [1-9][0-9]*			{ read $ $1 : $2 }
	/ '0'				{ 0 }

string :: String = '\"' char* '\"'
char :: Char = [^\\\"] / '\\' esc
esc :: Char
	= 'n'						{ '\n' }
	/ 'r'						{ '\r' }
	/ '\\'						{ '\\' }
	/ 'SUB'						{ '\SUB' }

ln :: String = [A-Z][_a-zA-Z0-9]*			{ $1 : $2 }
sn :: String = [a-z][_a-zA-Z0-9]*			{ $1 : $2 }

sp :: () = (comm / [ \t])*				{ () }
emp :: () = (comm / lcomm / [ \n])*			{ () }
lcomm :: Char = '--' [^\n]* [\n]			{ ' ' }
comm :: Char = '{-' (!'{-' !'-}' . / comm)* '-}'	{ ' ' }

|]
