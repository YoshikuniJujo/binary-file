{-# LANGUAGE TemplateHaskell, TupleSections, PackageImports #-}

module File.Binary.Quote (Field(..), Binary(..), binary) where

import File.Binary.Parse (
	parse, Structure, sName, sDerive, sArgName, sArgType, sItems,
	SItem, argOf, valueOf, constant, Value, expression, rights')
import File.Binary.Classes (Field(..), Binary(..))
import Language.Haskell.TH (
	Q, DecsQ, ClauseQ, Clause, ExpQ, Exp(..), Name, FieldExp, StmtQ,
	dataD, recC, varStrictType, strictType, notStrict,
	instanceD, funD, clause, normalB, valD, tySynInstD, cxt,
	conT, appT, sigE, varP, tupP, condE, recConE, tupE, listE,
	appE, appsE, infixApp, varE, conE, litE, newName, integerL, stringL,
	doE, bindS, noBindS, letS, tildeP)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad (zipWithM, foldM)
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
top bs = let c = sName bs in do
	(r, fe) <- reading c (sArgName bs) (sItems bs)
	(\d i -> [d, i])
	    <$> dataD (cxt []) c [] [recC c $
		map (varStrictType <$> fst <*> strictType notStrict . snd) $
			rights' $ map valueOf $ sItems bs] (sDerive bs)
	    <*> instanceD (cxt []) (appT (conT ''Field) (conT c)) [
		tySynInstD ''FieldArgument [conT c] $ sArgType bs,
		funD 'fromBits $ (: []) $ return r,
		funD 'consToBits $ (: []) $ writing fe (sArgName bs) (sItems bs)]

reading :: Name -> String -> [SItem] -> Q (Clause, [FieldExp])
reading con argn is = do
	arg <- newName "_arg"
	b <- newName "bin"
	(r, fe) <- readfs' con is (varE b)
		(\ret -> expression ret (varE arg) argn . argOf) undefined
	c <- flip (clause [varP arg, varP b]) [] $ normalB $ return r
	return (c, fe)

readfs' :: Name -> [SItem] -> ExpQ -> ([FieldExp] -> SItem -> ExpQ) -> ExpQ ->
	Q (Exp, [FieldExp])
readfs' con items bin size _ = do
	((binds, (rest, rts2)), rts) <- runWriterT $ (`runStateT` (bin, [])) $
		(zipWithM readf' <$> map (flip size) <*> map valueOf) items
--	let binds' = map (uncurry bindS) binds
	l <- doE $ concat binds ++ [noBindS $
		appE (varE 'return) $ tupE $ (: [rest]) $ recConE con $
			return <$> rts ]
	return (l, rts2)

type FieldMonad = StateT (ExpQ, [FieldExp]) (WriterT [FieldExp] Q)

liftQ :: Q a -> FieldMonad a
liftQ = lift . lift

liftW :: WriterT [FieldExp] Q a -> FieldMonad a
liftW = lift

readf' :: ([FieldExp] -> ExpQ) -> Value -> FieldMonad [StmtQ]
readf' size (Left val, typ) = do
	(bin, rts2) <- get
	[rv, rest, bin'] <- liftQ $ mapM newName ["_rv", "_rst", "_bin'"]
	put (varE bin', rts2)
	let lit = constant
		((`sigE` typ) . litE . integerL)
		(appE (varE 'pack) . litE . stringL)
		(\b -> conE (if b then 'True else 'False))
		val
	return [bindS (tupP [varP rv, varP rest])
				(appsE [varE 'fromBits, size rts2, bin]),
		noBindS $ condE (infixApp (varE rv) (varE '(==)) lit) [e| return () |]
			[e| fail "bad value" |],
		letS [flip (valD $ varP bin') [] $ normalB $
		condE (infixApp (varE rv) (varE '(==)) lit) (varE rest)
			[e| error "bad value" |]]]
readf' size (Right var, _) = do
	(bin, rts2) <- get
	[bin', tmp] <- liftQ $ mapM newName ["_bin'", "_tmp"]
	put (varE bin', (var, VarE tmp) : rts2)
	liftW $ tell [(var, VarE tmp)]
	return [bindS (tildeP $ tupP [varP tmp, varP bin']) $
		appsE [varE 'fromBits, size rts2, bin]]

writing :: [FieldExp] -> String -> [SItem] -> ClauseQ
writing fe argn items = do
	[arg, dat, bin0] <- mapM newName ["_arg", "_dat", "bin0"]
	let fe' = map ((\n -> (n, VarE n `AppE` VarE dat)) . fst) fe
	flip (clause [varP arg, varP dat, varP bin0]) [] $ normalB $
		appE (appsE [varE 'foldM, varE 'flip `appE` varE '($), varE bin0]) $
			(varE 'reverse `appE`) $ listE $ (<$> items) $ writef dat
				<$> expression fe' (varE arg) argn . argOf
				<*> valueOf

writef :: Name -> ExpQ -> Value -> ExpQ
writef _ size (Left val, typ) = varE 'consToBits `appE` size `appE` constant
	((`sigE` typ) . litE . integerL)
	(appE (varE 'pack) . litE . stringL)
	(\b -> conE (if b then 'True else 'False))
	val
writef dat size (Right rec, _) =
	varE 'consToBits `appE` size `appE` (varE rec `appE` varE dat)
