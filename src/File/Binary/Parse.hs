{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, PackageImports #-}

{-# OPTIONS_GHC
	-fno-warn-name-shadowing
	-fno-warn-unused-binds
	-fno-warn-unused-matches
	-fno-warn-unused-do-bind #-}

module File.Binary.Parse (
	parse, BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsItem,
	BSItem, argOf, valueOf, Value(..), variables, Expression, expression,
) where

import Control.Applicative ((<$>), (<*>))
import "monads-tf" Control.Monad.Reader (Reader, runReader, ask)
import Data.Maybe (catMaybes)
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
	bsArgName :: String,
	bsArgType :: TypeQ,
	bsItem :: [BSItem] }

data BSItem = BSItem { argOf :: Expression, typeOf :: TypeQ, valueOf :: Value }
type Expression	= Reader (ExpQ, ExpQ, String) ExpQ

expression :: ExpQ -> ExpQ -> String -> Expression -> ExpQ
expression ret arg argn e = runReader e (ret, arg, argn)

data Value = Constant (Either Integer String) | Variable Name

variables :: [BSItem] -> [(Name, TypeQ)]
variables = catMaybes . map (\bsi -> case valueOf bsi of
		Variable var -> Just (var, typeOf bsi); _ -> Nothing)

varToExp :: String -> (ExpQ, ExpQ, String) -> ExpQ
varToExp var (ret, arg, argn)
	| var == argn = arg
	| '.' `elem` var = varE $ mkName var
	| otherwise = appE (varE $ mkName var) ret

[peggy|

top :: BinaryStructure
	= emp lname der arg dat*
				{ BinaryStructure $2 $3 (fst $4) (snd $4) $5 }

der :: [Name]
	= emp 'deriving' sp ln (sp ',' sp ln)*
				{ mkName $3 : map (\(_, _, n) -> mkName n) $4 }
	/ ''			{ [] }

arg :: (String, TypeQ)
	= emp var sp '::' sp typ	{ ($2, $5) }
	/ ''				{ ("_", conT $ mkName "()") }

dat :: BSItem
	= emp ex sp typS sp ':' sp val	{ BSItem $2 $4 $7 }

typS :: TypeQ
	= '{' typ '}'			{ $1 }
	/ ''				{ conT $ mkName "Int" }

val :: Value
	= var				{ Variable $ mkName $1 }
	/ num				{ Constant $ Left $1 }
	/ string			{ Constant $ Right $1 }

ex :: Expression
	= ex sp op sp exOp1		{ uInfixE <$> $1 <*> $3 <*> $5 }
	/ exOp1

exOp1 :: Expression
	= exOp1 sp ex1			{ appE <$> $1 <*> $3 }
	/ ex1

ex1 :: Expression
	= '(' tupExp ')'
	/ '(' ex ')'			{ parensE <$> $1 }
	/ '()'				{ return $ conE $ mkName "()" }
	/ num				{ return $ litE $ integerL $1 }
	/ lname				{ return $ conE $1 }
	/ var				{ flip fmap ask $ varToExp $1 }

op :: Expression
	= [!\\#$%&*+./<=>?@^|~-:]+	{ return $ varE $ mkName $1 }
	/ '`' var '`'			{ return $ varE $ mkName $1 }
tupExp :: Expression = ex (sp ',' sp ex)+
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

var :: String
	= (ln '.')* sn			{ concatMap (++ ".") $1 ++ $2 }

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
