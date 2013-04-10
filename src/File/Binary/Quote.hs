{-# LANGUAGE TemplateHaskell, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import File.Binary.Parse (
	parse, Structure, sName, sDerive, sArgName, sArgType, sItems,
	SItem, argOf, valueOf, constant, Value, expression)
import File.Binary.Classes (Field(..), Binary(..))
import Language.Haskell.TH (
	Q, DecsQ, ClauseQ, BodyQ, ExpQ, Dec, Exp(..), Name, FieldExp,
	dataD, recC, varStrictType, strictType, notStrict,
	instanceD, funD, clause, normalB, valD, tySynInstD, cxt,
	conT, appT, sigE, varP, tupP, letE, condE, recConE, tupE, listE,
	appE, appsE, infixApp, varE, conE, litE, newName, integerL, stringL)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad (zipWithM)
import "monads-tf" Control.Monad.State (StateT, runStateT, get, put, lift)
import "monads-tf" Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Applicative ((<$>), (<*>))
import Data.Either (rights)

--------------------------------------------------------------------------------

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined, quotePat = undefined, quoteType = undefined,
	quoteDec = top . parse }

top :: Structure -> DecsQ
top bs = let c = sName bs in (\d i -> [d, i])
	<$> dataD (cxt []) c [] [recC c $
		map (varStrictType <$> fst <*> strictType notStrict . snd) $
			rights $ map valueOf $ sItems bs] (sDerive bs)
	<*> instanceD (cxt []) (appT (conT ''Field) (conT c)) [
		tySynInstD ''FieldArgument [conT c] $ sArgType bs,
		funD 'fromBits $ (: []) $ reading c (sArgName bs) (sItems bs),
		funD 'consToBits $ (: []) $ writing (sArgName bs) (sItems bs)]

reading :: Name -> String -> [SItem] -> ClauseQ
reading c argn is = do
	arg <- newName "_arg"
	b <- newName "bin"
	flip (clause [varP arg, varP b]) [] $ letRec $ readfs c is (varE b) $
		\ret -> expression ret (varE arg) argn . argOf

letRec :: (ExpQ -> ExpQ) -> BodyQ
letRec e = normalB $ do
	(ret, rest) <- (,) <$> newName "ret" <*> newName "rest"
	letE [valD (tupP [varP ret, varP rest]) (normalB $ e $ varE ret) []] $
		tupE [varE ret, varE rest]

readfs :: Name -> [SItem] -> ExpQ -> (ExpQ -> SItem -> ExpQ) -> ExpQ -> ExpQ
readfs con items bin size ret = do
	((binds, rest), rts) <- runWriterT $ (`runStateT` bin) $
		(zipWithM readf <$> map (size ret) <*> map valueOf) items
	letE (return <$> binds) $ tupE $ (: [rest]) $ recConE con $ return <$> rts

type FieldMonad = StateT ExpQ (WriterT [FieldExp] Q)

liftQ :: Q a -> FieldMonad a
liftQ = lift . lift

liftW :: WriterT [FieldExp] Q a -> FieldMonad a
liftW = lift

readf :: ExpQ -> Value -> FieldMonad Dec
readf size (Left val) = do
	bin <- get
	[rv, rest, bin'] <- liftQ $ mapM newName ["rv", "rst", "bin'"]
	put $ varE bin'
	let lit = constant
		((`sigE` conT ''Integer) . litE . integerL)
		(appE (varE 'pack) . litE . stringL)
		(\b -> if b then conE 'True else conE 'False)
		val
	liftQ $ flip (valD $ varP bin') [] $ normalB $
		letE [flip (valD $ tupP [varP rv, varP rest]) [] $ normalB $
			appsE [varE 'fromBits, size, bin]] $
		condE (infixApp (varE rv) (varE '(==)) lit) (varE rest)
			[e| error "bad value" |]
readf size (Right (var, _)) = do
	bin <- get
	[bin', tmp] <- liftQ $ mapM newName ["bin'", "tmp"]
	put $ varE bin'
	liftW $ tell [(var, VarE tmp)]
	liftQ $ valD (tupP [varP tmp, varP bin'])
		(normalB $ appsE [varE 'fromBits, size, bin]) []

writing :: String -> [SItem] -> ClauseQ
writing argn items = do
	[arg, dat, bin0] <- mapM newName ["_arg", "_dat", "bin0"]
	flip (clause [varP arg, varP dat, varP bin0]) [] $ normalB $
		appE (appsE [varE 'foldr, varE '($), varE bin0]) $ listE $
			(<$> items) $ writef dat
				<$> expression (varE dat) (varE arg) argn . argOf
				<*> valueOf

writef :: Name -> ExpQ -> Value -> ExpQ
writef _ size (Left val) = varE 'consToBits `appE` size `appE` constant
	((`sigE` conT ''Integer) . litE . integerL)
	(appE (varE 'pack) . litE . stringL)
	(\b -> if b then conE 'True else conE 'False)
	val
writef dat size (Right (rec, _)) =
	varE 'consToBits `appE` size `appE` (varE rec `appE` varE dat)
