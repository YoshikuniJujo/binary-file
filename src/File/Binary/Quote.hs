{-# LANGUAGE TemplateHaskell, PatternGuards, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import Language.Haskell.TH (
	Q, Name, newName, varP, tupP, integerL, stringL,
	ExpQ, varE, litE, appE, appsE, tupE, letE, condE, sigE, listE, recConE,
	TypeQ, appT, conT, infixApp,
	DecsQ, DecQ, Dec, valD, dataD, funD, instanceD, tySynInstD, ClauseQ,
	normalB, cxt, clause, recC, varStrictType, strictType, notStrict)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.Traversable (for)
import Data.Monoid (mconcat)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (ByteString, pack)
import "monads-tf" Control.Monad.State

import File.Binary.Parse (
	parse,
	BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsBody,
	BSItem, bytesOf, valueOf,
	Value(..), variables,
	expression)
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
		funD 'fromBinary $ (: []) $ reading bsn argn body,
		funD 'toBinary $ (: []) $ writing argn body
	 ]

reading :: Name -> Name -> [BSItem] -> ClauseQ
reading bsn argn body = do
	arg <- newName "_arg"
	bin <- newName "bin"
	flip (clause [varP arg, varP bin]) [] $
		normalB $ mkLetRec $ mkBody bsn (varE arg) argn body (varE bin)

mkLetRec :: (ExpQ -> ExpQ) -> ExpQ
mkLetRec e = do
	(ret, rest) <- (,) <$> newName "ret" <*> newName "rest"
	letE [valD (tupP [varP ret, varP rest]) (normalB $ e $ varE ret) []] $
		tupE [varE ret, varE rest]

mkBody :: Name -> ExpQ -> Name -> [BSItem] -> ExpQ -> ExpQ -> ExpQ
mkBody bsn arg argn body bin ret = do
	nps <- for (variables body) $ \(n, _) -> (n ,) <$> newName "tmp"
	(defs, rest) <- (`runStateT` bin) $
		for body $ mkDef nps $ expression ret arg argn . bytesOf
	letE (return <$> defs) $ tupE $ (: [rest]) $ recConE bsn $ (<$> nps) $
		\(n, tmp) -> (n ,) <$> varE tmp

mkDef :: [(Name, Name)] -> (BSItem -> ExpQ) -> BSItem -> StateT ExpQ Q Dec
mkDef np getn item
	| Constant val <- valueOf item = do
		bin <- get
		fv <- lift $ newName "fv"
		ret <- lift $ newName "rt"
		bin' <- lift $ newName "bin'"
		put $ varE bin'
		let (lit, sig) = either
			((, conT ''Integer) . litE . integerL)
			((, conT ''BSLC.ByteString) .
				appE (varE 'BSLC.pack) . litE . stringL) val
		lift $ flip (valD $ varP bin') [] $ normalB $
			letE [flip (valD $ tupP [varP fv, varP ret]) [] $ normalB $
					appsE [varE 'fromBinary, getn item, bin]] $
			condE (infixApp (varE fv) (varE '(==))
					(lit `sigE` sig))
				(varE ret)
				[e| error "bad value" |]
	| Variable var <- valueOf item = do
		bin <- get
		bin' <- lift $ newName "bin'"
		put $ varE bin'
		lift $ valD (tupP [varP $ fromJust $ lookup var np, varP bin'])
			(normalB $ appE (appE (varE 'fromBinary) $ getn item) bin)
			[]
	| otherwise = error "never occur"

writing :: Name -> [BSItem] -> ClauseQ
writing argn body = do
	arg <- newName "_arg"
	bin <- newName "_bin"
	let size = expression (varE bin) (varE arg) argn . bytesOf 
	flip (clause [varP arg, varP bin]) [] $ normalB $
		appE (varE 'mconcat) $ listE $ (<$> body) $
			writeField bin <$> size <*> valueOf

writeField :: Name -> ExpQ -> Value -> ExpQ
writeField _ size (Constant val) =  varE 'toBinary `appE` size `appE` either
	((`sigE` conT ''Integer) . litE . integerL)
	((`sigE` conT ''BSLC.ByteString) . appE (varE 'BSLC.pack) . litE . stringL)
	val
writeField bin size (Variable v) =
	varE 'toBinary `appE` size `appE` (varE v `appE` varE bin)
