{-# LANGUAGE TemplateHaskell, PatternGuards, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import Language.Haskell.TH (
	Q, Name, newName, varP, tupP, integerL, stringL,
	ExpQ, varE, litE, appE, appsE, infixE, tupE, letE, condE, sigE, listE, recConE,
	TypeQ, appT, conT,
	DecsQ, DecQ, Dec, valD, dataD, funD, instanceD, tySynInstD,
	normalB, cxt, clause, recC, varStrictType, strictType, notStrict)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.Traversable (for)
import Data.Monoid (mconcat)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import "monads-tf" Control.Monad.State

import File.Binary.Parse (
	parse,
	BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsBody,
	BSItem, bytesOf, valueOf,
	Value(..), variables,
	Expression, expression)
import File.Binary.Classes (Field(..), Binary(..))

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = mkHaskellTree . parse
 }

mkHaskellTree :: BinaryStructure -> DecsQ
mkHaskellTree bs = (\d i -> [d, i]) <$>
	dataD (cxt []) bsn [] [recC bsn $ fields $ variables body] ders <*>
	mkInst bsn argn typ body
	where
	bsn = bsName bs
	ders = bsDerive bs
	argn = bsArgName bs
	typ = bsArgType bs
	body = bsBody bs
	fields = map $ varStrictType <$> fst <*> strictType notStrict . snd

mkInst :: Name -> Name -> TypeQ -> [BSItem] -> DecQ
mkInst bsn argn typ body =
	instanceD (cxt []) (appT (conT ''Field) (conT bsn)) [
		tySynInstD ''FieldArgument [conT bsn] typ,
		reading 'fromBinary bsn argn body,
		writing 'toBinary argn body
	 ]

reading :: Name -> Name -> Name -> [BSItem] -> DecQ
reading name bsn argn body = do
	arg <- newName "_arg"
	bin <- newName "bin"
	funD name [clause [varP arg, varP bin]
		(normalB $ mkLetRec $ mkBody bsn (varE arg) argn body bin) []]

mkLetRec :: (ExpQ -> ExpQ) -> ExpQ
mkLetRec e = do
	(ret, rest) <- (,) <$> newName "ret" <*> newName "rest"
	letE [valD (tupP [varP ret, varP rest]) (normalB $ e $ varE ret) []] $
		tupE [varE ret, varE rest]

mkBody :: Name -> ExpQ -> Name -> [BSItem] -> Name -> ExpQ -> ExpQ
mkBody bsn arg argn body bin ret = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName "tmp"
	(defs, rest) <- mapM (mkDef getN namePairs) body `runStateT` bin
	letE (map return defs) $ tupE
		[recConE bsn (map toPair namePairs), varE rest]
	where
	names = map fst $ variables body
	toPair (n, nn) = (n, ) <$> varE nn
	getN = expression ret arg argn . bytesOf

mkDef :: (BSItem -> ExpQ) -> [(Name, Name)] -> BSItem -> StateT Name Q Dec
mkDef getN np item
    | Constant (Left val) <- valueOf item = do
	cs' <- get
	cs'' <- lift $ newName "cs"
	let	t = dropE' (getN item) $ varE cs'
		p = val `equal` appE (varE 'fst)
			(appE (appE (varE 'fromBinary) $ getN item) $
				takeE' (getN item) $ varE cs')
		e = [e| error "bad value" |]
	d <- lift $ valD (varP cs'') (normalB $ condE p t e) []
	put cs''
	return d
    | Constant (Right val) <- valueOf item = do
	cs' <- get
	cs'' <- lift $ newName "cs"
	let t = dropE' (getN item) $ varE cs'
	let p = val `equal'` takeE' (getN item) (varE cs')
	let e = [e| error "bad value" |]
	d <- lift $ valD (varP cs'') (normalB $ condE p t e) []
	put cs''
	return d
    | Variable var <- valueOf item = do
	cs' <- get
	cs'' <- lift $ newName "cs"
	def <- lift $ valD (tupP [varP $ fromJust $ lookup var np, varP cs''])
		(normalB $ appE (appE (varE 'fromBinary) $ getN item) $ varE cs') []
	put cs''
	return def
    | otherwise = error "bad"

writing :: Name -> Name -> [BSItem] -> DecQ
writing name argn body = do
	arg <- newName "_arg"
	bs <- newName "_bs"
	let run = appE (varE 'mconcat) $ listE $ map
		(\bsi -> writeField bs arg argn (bytesOf bsi) (valueOf bsi)) body
	funD name
		[clause [varP arg, varP bs] (normalB run) []]

writeField :: Name -> Name -> Name -> Expression -> Value -> ExpQ
writeField bs arg argn size (Constant (Left n)) =
	appsE [fiend', expression (varE bs) (varE arg) argn size,
		sigE (litE $ integerL $ fromIntegral n)
		(conT ''Int)]
	where
	fiend' = varE 'toBinary
writeField _ _ _ _ (Constant (Right s)) =
	appsE [varE 'fs, litE $ stringL s]
writeField bs arg argn bytes (Variable v) =
	fieldValueToStr bs arg argn bytes False $ getField bs v

fs :: Binary a => String -> a
fs = makeBinary . BSLC.pack

fieldValueToStr :: Name -> Name -> Name -> Expression -> Bool -> ExpQ -> ExpQ
fieldValueToStr bs arg argn size False =
	appE $ appE (varE 'toBinary) (expression (varE bs) (varE arg) argn size)
fieldValueToStr bs arg argn size True = \val ->
	appE (varE 'mconcat) $ appsE [
		varE 'map, appE (varE 'toBinary) (expression (varE bs) (varE arg) argn size), val]

getField :: Name -> Name -> ExpQ
getField bs v = appE (varE v) (varE bs)

equal :: Integer -> ExpQ -> ExpQ
equal x y = infixE (Just $ sigE (litE $ integerL x) (conT ''Integer))
	(varE '(==)) (Just y)

equal' :: String -> ExpQ -> ExpQ
equal' x y = infixE (Just $ litE $ stringL x) (varE '(==)) (Just y)

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = -- appE (varE 'ts) $ appsE [varE 'tk, n, xs]
	appE (varE 'BSLC.unpack) $ appE (varE 'fst) $ appsE [varE 'getBytes, n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'dp, n, xs]

dp :: Binary a => Int -> a -> a
dp n = snd . getBytes n
