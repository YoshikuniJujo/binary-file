{-# LANGUAGE
	TemplateHaskell,
	TupleSections,
	PatternGuards,
	TypeSynonymInstances,
	FlexibleInstances #-}

module QuoteBinaryStructure (
	binary,
	Field(..),
	Binary(..),
	fii, -- fiiBE,
	tii, -- tiiBE,
	times
) where

import Prelude hiding (sequence)

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Data.Traversable hiding (mapM)
import Data.Either
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BSLC

import ParseBinaryStructure

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = mkHaskellTree . parseBinaryStructure
 }

mkHaskellTree :: BinaryStructure -> DecsQ
mkHaskellTree bs = do
		d <- mkData bsn body
		i <- mkInst bsn argn typ body
		return $ d ++ [i]
	where
	bsn = binaryStructureName bs
	argn = binaryStructureArgName bs
	typ = binaryStructureArgType bs
	body = binaryStructureBody bs

mkInst :: String -> String -> TypeQ -> [BinaryStructureItem] -> DecQ
mkInst bsn argn typ body =
	instanceD (cxt []) (appT (conT ''Field) (conT $ mkName bsn)) [
		tySynInstD ''FieldArgument [conT $ mkName bsn] typ,
		reading "fromBinary" bsn argn body,
		writing "toBinary" argn body
	 ]

writing :: String -> String -> [BinaryStructureItem] -> DecQ
writing name argn body = do
	arg <- newName "_arg"
	bs <- newName "_bs"
	let run = appE (varE 'cc) $ listE $ map
		(\bsi -> writeField bs arg argn (bytesOf bsi) (valueOf bsi)) body
	funD (mkName name)
		[clause [varP arg, varP bs] (normalB run) []]

writeField :: Name -> Name -> String -> Expression -> Either (Either Int String) String -> ExpQ
writeField bs arg argn size (Left (Left n)) =
	appsE [fiend', expression bs arg argn size, sigE (litE $ integerL $ fromIntegral n)
		(conT ''Int)]
	where
	fiend' = varE 'toBinary
writeField _ _ _ _ (Left (Right s)) =
	appsE [varE 'fs, litE $ stringL s]
writeField bs arg argn bytes (Right v) =
	fieldValueToStr bs arg argn bytes False $ getField bs v

fieldValueToStr :: Name -> Name -> String -> Expression -> Bool -> ExpQ -> ExpQ
fieldValueToStr bs arg argn size False =
	appE $ appE (varE 'toBinary) (expression bs arg argn size)
fieldValueToStr bs arg argn size True = \val ->
	appE (varE 'cc) $ appsE [
		varE 'map, appE (varE 'toBinary) (expression bs arg argn size), val]

reading :: String -> String -> String -> [BinaryStructureItem] -> DecQ
reading name bsn argn body = do
	arg <- newName "_arg"
	cs <- newName "cs1"
	ret <- newName "ret"
	funD (mkName name) [clause [varP arg, varP cs]
		(normalB $ mkLetRec ret $ mkBody bsn arg argn body cs) []]

mkLetRec :: Name -> (Name -> ExpQ) -> ExpQ
mkLetRec n f = do
	rest <- newName "rest"
	letE [valD (tupP [varP n, varP rest]) (normalB $ f n) []] $
		tupE [varE n, varE rest]

mkBody :: String -> Name -> String -> [BinaryStructureItem] -> Name -> Name -> ExpQ
mkBody bsn arg argn body cs ret = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName "tmp"
	(defs, rest) <- gather cs body $ mkDef namePairs
	letE (map return defs) $ tupE
		[recConE (mkName bsn) (map toPair2 namePairs), varE rest]
	where
	names = rights $ map valueOf body
	toPair2 (n, nn) = return (mkName n, VarE nn)
	mkDef :: [(String, Name)] -> BinaryStructureItem -> Name -> Q ([Dec], Name)
	mkDef np item cs'
	    | Left (Left val) <- valueOf item = do
		cs'' <- newName "cs"
		let	t = dropE' n $ varE cs'
			p = val `equal` appE (varE 'fst)
				(appE (appE (varE 'fromBinary) arg') $
					takeE' n $ varE cs')
			e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Left (Right val) <- valueOf item = do
		cs'' <- newName "cs"
		let t = dropE' n $ varE cs'
		let p = val `equal'` takeE' n (varE cs')
		let e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Right var <- valueOf item = do
		cs'' <- newName "cs"
		def <- valD (tupP [varP $ fromJust $ lookup var np, varP cs''])
			(normalB $ appE (appE (varE 'fromBinary) arg') $ varE cs') []
		return ([def], cs'')
	    | otherwise = error "bad"
	    where
	    n = expression ret arg argn $ bytesOf item
	    arg' = expression ret arg argn $ bytesOf item

getField :: Name -> String -> ExpQ
getField bs v = appE (varE $ mkName v) (varE bs)

equal :: Int -> ExpQ -> ExpQ
equal x y = infixE (Just $ sigE (litE $ integerL $ fromIntegral x) (conT ''Int))
	(varE '(==)) (Just y)

equal' :: String -> ExpQ -> ExpQ
equal' x y = infixE (Just $ litE $ stringL x) (varE '(==)) (Just y)

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = -- appE (varE 'ts) $ appsE [varE 'tk, n, xs]
	appE (varE 'BSLC.unpack) $ appE (varE 'fst) $ appsE [varE 'getBytes, n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'dp, n, xs]

gather :: Monad m => s -> [a] -> (a -> s -> m ([b], s)) -> m ([b], s)
gather s [] _ = return ([], s)
gather s (x : xs) f = do
	(ys, s') <- f x s
	(zs, s'') <- gather s' xs f
	return (ys ++ zs, s'')

mkData :: String -> [BinaryStructureItem] -> DecsQ
mkData bsn body = do
	d <- dataD (cxt []) name [] [con] [''Show]
	return [d]
	where
	name = mkName bsn
	con = recC (mkName bsn) vsts

	vsts = flip map (filter isRight body) $ \item ->
		varStrictType (mkName $ fromRight $ valueOf item) $
			strictType notStrict $ mkType False $ typeOf item
	isRight item
		| Right _ <- valueOf item = True
		| otherwise = False

mkType :: Bool -> TypeQ -> TypeQ
mkType True t = appT listT $ mkType False t
mkType False typ = typ

fromRight :: Either a b -> b
fromRight = either (error "not Right") id

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times 0 _ s = ([], s)
times n f s = let
	(ret, rest) = f s
	(rets, rest') = times (n - 1) f rest in
	(ret : rets, rest')
