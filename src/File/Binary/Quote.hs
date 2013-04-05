{-# LANGUAGE TemplateHaskell, PatternGuards, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import Language.Haskell.TH (
	Q, DecsQ, ClauseQ, ExpQ, Dec, Name,
	dataD, recC, varStrictType, strictType, notStrict,
	instanceD, funD, clause, normalB, valD, tySynInstD, cxt,
	conT, appT, sigE, varP, tupP, letE, condE, recConE, tupE, listE,
	appE, appsE, infixApp, varE, litE, newName, integerL, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Traversable (for)
import "monads-tf" Control.Monad.State (StateT, runStateT, get, put, lift)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (ByteString, pack)

import File.Binary.Parse (
	parse, BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsItem,
	BSItem, bytesOf, valueOf, Value(..), variables, expression)
import File.Binary.Classes (Field(..), Binary(..))

--------------------------------------------------------------------------------

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined, quotePat = undefined, quoteType = undefined,
	quoteDec = top . parse
 }

top :: BinaryStructure -> DecsQ
top bs = let c = bsName bs in (\d i -> [d, i])
	<$> dataD (cxt []) c [] [recC c $
		map (varStrictType <$> fst <*> strictType notStrict . snd) $
			variables $ bsItem bs] (bsDerive bs)
	<*> instanceD (cxt []) (appT (conT ''Field) (conT c)) [
		tySynInstD ''FieldArgument [conT c] $ bsArgType bs,
		funD 'fromBinary $ (: []) $ reading c (bsArgName bs) (bsItem bs),
		funD 'toBinary $ (: []) $ writing (bsArgName bs) (bsItem bs)
	 ]

reading :: Name -> Name -> [BSItem] -> ClauseQ
reading con argn items = do
	arg <- newName "_arg"
	bin <- newName "bin"
	flip (clause [varP arg, varP bin]) [] $
		normalB $ letRec $ binToDat con items (varE bin) $ \ret ->
			expression ret (varE arg) argn . bytesOf

letRec :: (ExpQ -> ExpQ) -> ExpQ
letRec e = do
	(ret, rest) <- (,) <$> newName "ret" <*> newName "rest"
	letE [valD (tupP [varP ret, varP rest]) (normalB $ e $ varE ret) []] $
		tupE [varE ret, varE rest]

binToDat :: Name -> [BSItem] -> ExpQ -> (ExpQ -> BSItem -> ExpQ) -> ExpQ -> ExpQ
binToDat con items bin size ret = do
	rts <- for (variables items) $ \(r, _) -> (r ,) <$> newName "tmp"
	(binds, rest) <- for items (binToField rts $ size ret) `runStateT` bin
	letE (return <$> binds) $ tupE $ (: [rest]) $ recConE con $ (<$> rts) $
		\(r, tmp) -> (r ,) <$> varE tmp

binToField :: [(Name, Name)] -> (BSItem -> ExpQ) -> BSItem -> StateT ExpQ Q Dec
binToField rt size item
	| Constant val <- valueOf item = do
		bin <- get
		rv <- lift $ newName "rv"
		rest <- lift $ newName "rst"
		bin' <- lift $ newName "bin'"
		put $ varE bin'
		let v = either
			((`sigE` conT ''Integer) . litE . integerL)
			((`sigE` conT ''BSLC.ByteString) .
				appE (varE 'BSLC.pack) . litE . stringL) val
		lift $ flip (valD $ varP bin') [] $ normalB $
			letE [flip (valD $ tupP [varP rv, varP rest]) [] $ normalB $
				appsE [varE 'fromBinary, size item, bin]] $
			condE (infixApp (varE rv) (varE '(==)) v) (varE rest)
				[e| error "bad value" |]
	| Variable var <- valueOf item = do
		bin <- get
		bin' <- lift $ newName "bin'"
		put $ varE bin'
		lift $ valD (tupP [varP $ fromJust $ lookup var rt, varP bin'])
			(normalB $ appE (appE (varE 'fromBinary) $ size item) bin)
			[]
	| otherwise = error "never occur"

writing :: Name -> [BSItem] -> ClauseQ
writing argn items = do
	arg <- newName "_arg"
	dat <- newName "_dat"
	flip (clause [varP arg, varP dat]) [] $ normalB $
		appE (varE 'mconcat) $ listE $ (<$> items) $ fieldToBin dat
			<$> expression (varE dat) (varE arg) argn . bytesOf
			<*> valueOf

fieldToBin :: Name -> ExpQ -> Value -> ExpQ
fieldToBin _ size (Constant val) =  varE 'toBinary `appE` size `appE` either
	((`sigE` conT ''Integer) . litE . integerL)
	((`sigE` conT ''BSLC.ByteString) . appE (varE 'BSLC.pack) . litE . stringL)
	val
fieldToBin dat size (Variable val) =
	varE 'toBinary `appE` size `appE` (varE val `appE` varE dat)
