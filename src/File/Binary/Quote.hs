{-# LANGUAGE TemplateHaskell, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import Language.Haskell.TH (
	Q, DecsQ, ClauseQ, ExpQ, Dec, Exp(..), Name, FieldExp,
	dataD, recC, varStrictType, strictType, notStrict,
	instanceD, funD, clause, normalB, valD, tySynInstD, cxt,
	conT, appT, sigE, varP, tupP, letE, condE, recConE, tupE, listE,
	appE, appsE, infixApp, varE, litE, newName, integerL, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Control.Monad (zipWithM)
import "monads-tf" Control.Monad.State (StateT, runStateT, get, put, lift)
import "monads-tf" Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (ByteString, pack)

import File.Binary.Parse (
	parse, BinaryStructure, bsName, bsDerive, bsArgName, bsArgType, bsItem,
	BSItem, argOf, valueOf, Value(..), variables, expression)
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
		funD 'fromBitsBinary $ (: []) $ reading c (bsArgName bs) (bsItem bs),
		funD 'consToBitsBinary $ (: []) $ writing (bsArgName bs) (bsItem bs)
	 ]

reading :: Name -> String -> [BSItem] -> ClauseQ
reading con argn items = do
	arg <- newName "_arg"
	bin <- newName "bin"
	flip (clause [varP arg, varP bin]) [] $
		normalB $ letRec $ binToDat con items
--			(appsE[conE $ '(,), conE '[], varE bin]) $ \ret ->
			(varE bin) $ \ret ->
			expression ret (varE arg) argn . argOf

letRec :: (ExpQ -> ExpQ) -> ExpQ
letRec e = do
	(ret, rest) <- (,) <$> newName "ret" <*> newName "rest"
	letE [valD (tupP [varP ret, varP rest]) (normalB $ e $ varE ret) []] $
		tupE [varE ret, varE rest]

binToDat :: Name -> [BSItem] -> ExpQ -> (ExpQ -> BSItem -> ExpQ) -> ExpQ -> ExpQ
binToDat con items bin size ret = do
	((binds, rest), rts) <- runWriterT $ (`runStateT` bin) $
		(zipWithM binToField <$> map (size ret) <*> map valueOf) items
	letE (return <$> binds) $ tupE $ (: [rest]) $ recConE con $ return <$> rts

type FieldMonad = StateT ExpQ (WriterT [FieldExp] Q)

liftQ :: Q a -> FieldMonad a
liftQ = lift . lift

liftW :: WriterT [FieldExp] Q a -> FieldMonad a
liftW = lift

binToField :: ExpQ -> Value -> FieldMonad Dec
binToField size (Constant val) = do
	bin <- get
	[rv, rest, bin'] <- liftQ $ mapM newName ["rv", "rst", "bin'"]
	put $ varE bin'
	let lit = either
		((`sigE` conT ''Integer) . litE . integerL)
		((`sigE` conT ''BSLC.ByteString) .
			appE (varE 'BSLC.pack) . litE . stringL) val
	liftQ $ flip (valD $ varP bin') [] $ normalB $
		letE [flip (valD $ tupP [varP rv, varP rest]) [] $ normalB $
			appsE [varE 'fromBitsBinary, size, bin]] $
		condE (infixApp (varE rv) (varE '(==)) lit) (varE rest)
			[e| error "bad value" |]
binToField size (Variable var) = do
	bin <- get
	[bin', tmp] <- liftQ $ mapM newName ["bin'", "tmp"]
	put $ varE bin'
	liftW $ tell [(var, VarE tmp)]
	liftQ $ valD (tupP [varP tmp, varP bin'])
		(normalB $ appsE [varE 'fromBitsBinary, size, bin]) []

writing :: String -> [BSItem] -> ClauseQ
writing argn items = do
	arg <- newName "_arg"
	dat <- newName "_dat"
	bin0 <- newName "bin0"
	flip (clause [varP arg, varP dat, varP bin0]) [] $ normalB $
		appE (appsE [varE 'foldr, varE '($), varE bin0]) $
--			tupE [conE '[], varE bin0]]) $
		    listE $
			(<$> items) $ fieldToBin dat
			<$> expression (varE dat) (varE arg) argn . argOf
			<*> valueOf

fieldToBin :: Name -> ExpQ -> Value -> ExpQ
fieldToBin _ size (Constant val) =
    varE 'consToBitsBinary `appE` size `appE` either
	((`sigE` conT ''Integer) . litE . integerL)
	((`sigE` conT ''BSLC.ByteString) . appE (varE 'BSLC.pack) . litE . stringL)
	val
fieldToBin dat size (Variable val) =
	varE 'consToBitsBinary `appE` size `appE` (varE val `appE` varE dat)
