{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, PackageImports #-}

{-# OPTIONS_GHC
	-fno-warn-name-shadowing
	-fno-warn-unused-binds
	-fno-warn-unused-matches
	-fno-warn-unused-do-bind #-}

module File.Binary.Parse (
	parse, Structure, sName, sDerive, sArgName, sArgType, sItems,
	BSItem, argOf, valueOf, typeOf, Value(..), Expression, expression,
) where

import Text.Peggy (peggy, parseString, space, defaultDelimiter)
import Language.Haskell.TH (
	ExpQ, litE, varE, conE, appE, tupE, integerL, uInfixE, parensE,
	TypeQ, conT, appT, listT, tupleT, Name, mkName)
import "monads-tf" Control.Monad.Reader (Reader, runReader, ask)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Numeric (readHex)

--------------------------------------------------------------------------------

parse :: String -> Structure
parse = either (error . show) id . parseString top ""

data Structure = Structure {
	sName :: Name,
	sDerive :: [Name],
	sArgName :: String,
	sArgType :: TypeQ,
	sItems :: [BSItem] }

data BSItem = BSItem { argOf :: Expression, typeOf :: TypeQ, valueOf :: Value }
type Expression	= Reader (ExpQ, ExpQ, String) ExpQ

expression :: ExpQ -> ExpQ -> String -> Expression -> ExpQ
expression ret arg argn e = runReader e (ret, arg, argn)

data Value = Constant (Either Integer String) | Variable Name

identify :: String -> (ExpQ, ExpQ, String) -> ExpQ
identify var (ret, arg, argn)
	| var == argn = arg
	| '.' `elem` var = varE $ mkName var
	| otherwise = appE (varE $ mkName var) ret

[peggy|

top :: Structure
	= emp lname der arg dat*	{ Structure $2 $3 (fst $4) (snd $4) $5 }

der :: [Name]
	= emp 'deriving' sp ln (sp ',' sp ln)*
				{ mkName $3 : map (\(_, _, n) -> mkName n) $4 }
	/ ''			{ [] }

arg :: (String, TypeQ)
	= emp var sp '::' sp typ	{ ($2, $5) }
	/ ''				{ ("_", conT $ mkName "()") }

dat :: BSItem = emp ex? sp typS sp ':' sp val
	{ BSItem (fromMaybe (return $ conE $ mkName "()") $2) $4 $7 }

typS :: TypeQ = '{' typ '}' / ''	{ conT $ mkName "Int" }

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
	= '(' ex (sp ',' sp ex)+ ')'	{ (.) tupE . (:) <$> $1 <*>
						mapM (\(_, _, e) -> e) $2 }
	/ '(' ex ')'			{ parensE <$> $1 }
	/ '()'				{ return $ conE $ mkName "()" }
	/ num				{ return $ litE $ integerL $1 }
	/ lname				{ return $ conE $1 }
	/ var				{ identify $1 <$> ask }

op :: Expression
	= [!\\#$%&*+./<=>?@^|~-:]+	{ return $ varE $ mkName $1 }
	/ '`' var '`'			{ return $ varE $ mkName $1 }

typ :: TypeQ
	= typ typ1			{ appT $1 $2 }
	/ typ1

typ1 :: TypeQ
	= '(' typ sp (',' sp typ)+ ')'	{ foldl appT (tupleT $ length $3 + 1) $
						$1 : map snd $3 }
	/ '(' typ ')'
	/ '()'				{ conT $ mkName "()" }
	/ '[' typ ']'			{ appT listT $1 }
	/ lname				{ conT $1 }

lname :: Name = (ln '.')* ln		{ mkName $ concatMap (++ ".") $1 ++ $2 }

var :: String = (ln '.')* sn		{ concatMap (++ ".") $1 ++ $2 }

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
