{-# LANGUAGE
	TemplateHaskell,
	QuasiQuotes,
	FlexibleContexts,
	PackageImports,
	PatternGuards #-}

{-# OPTIONS_GHC
	-fno-warn-name-shadowing
	-fno-warn-unused-binds
	-fno-warn-unused-matches
	-fno-warn-unused-do-bind #-}

module File.Binary.Parse (
	parse, Structure, sName, sDerive, sArgName, sArgType, sItems,
	SItem, argOf, valueOf, constant, Value, expression, rights'
) where

import Text.Peggy (peggy, parseString, space, defaultDelimiter)
import Language.Haskell.TH (
	ExpQ, integerL, litE, varE, conE, tupE, appE, uInfixE, parensE,
	TypeQ, conT, listT, tupleT, appT, Name, mkName, FieldExp)
import "monads-tf" Control.Monad.Reader (Reader, runReader, ask)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow(first)
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
	sItems :: [SItem] }

data SItem = SItem { argOf :: Expression, valueOf :: Value }
type Expression	= Reader ([FieldExp], ExpQ, String) ExpQ

rights' :: [Value] -> [(Name, TypeQ)]
rights' = map (first fromRight) . filter (isRight . fst)

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "not Right"

isRight :: Either a b -> Bool
isRight (Right x) = True
isRight _ = False

expression :: [FieldExp] -> ExpQ -> String -> Expression -> ExpQ
expression ret arg argn e = runReader e (ret, arg, argn)

type Value = (Either Constant Name, TypeQ)

data Constant = Integer Integer | String String | Bool Bool deriving Show

constant :: (Integer -> a) -> (String -> a) -> (Bool -> a) -> Constant -> a
constant f _ _ (Integer i) = f i
constant _ f _ (String s) = f s
constant _ _ f (Bool b) = f b

identify :: String -> ([FieldExp], ExpQ, String) -> ExpQ
-- identify "_" _ = wildP
identify var (ret, arg, argn)
	| var == argn = arg
	| Just var' <- lookup (mkName var) ret = return var'
	| otherwise = varE $ mkName var

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

dat :: SItem = emp ex? sp typS sp ':' sp val
	{ SItem (fromMaybe (return $ conE $ mkName "()") $2) ($7, $4) }
--	{ SItem (fromMaybe (return $ conE $ mkName "()") $2) $
--		either Left (Right . second (const $4)) $7 }

typS :: TypeQ = '{' typ '}' / ''	{ conT $ mkName "Int" }

val :: Either Constant Name
	= var				{ Right $ mkName $1 }
	/ num				{ Left $ Integer $1 }
	/ string			{ Left $ String $1 }
	/ 'True'			{ Left $ Bool True }
	/ 'False'			{ Left $ Bool False }

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
	= [!\\#$%&*+./<=>?@^|~\-]+	{ return $ varE $ mkName $1 }
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
sn :: String = [_a-z][_a-zA-Z0-9]*			{ $1 : $2 }

sp :: () = (comm / [ \t])*				{ () }
emp :: () = (comm / lcomm / [ \n])*			{ () }
lcomm :: Char = '--' [^\n]* [\n]			{ ' ' }
comm :: Char = '{-' (!'{-' !'-}' . / comm)* '-}'	{ ' ' }

|]
