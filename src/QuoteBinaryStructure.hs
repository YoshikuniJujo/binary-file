{-# LANGUAGE
	TemplateHaskell,
	TupleSections,
	PatternGuards,
	TypeSynonymInstances,
	FlexibleInstances #-}

module QuoteBinaryStructure (
	binary,
	RetType(..),
	Str(..),
	fii, fiiBE,
	tii, tiiBE,
	times
) where

import Prelude hiding (sequence)

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Data.Traversable hiding (mapM)
import Data.Either
import Data.Maybe

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
		r <- mkReader bsn body
		w <- mkWriter bsn body
		return $ d ++ [r, w]
	where
	bsn = binaryStructureName bs
	body = binaryStructureBody bs

mkWriter :: String -> [BinaryStructureItem] -> DecQ
mkWriter bsn body = do
	bs <- newName "bs"
	let run = appE (varE 'cc) $ listE $ map
		(\bsi -> writeField bs (bytesOf bsi) (valueOf bsi)) body
	funD (mkName $ "write" ++ bsn)
		[clause [varP bs] (normalB run) []]

writeField :: Name -> Expression -> Either (Either Int String) String -> ExpQ
writeField bs size (Left (Left n)) =
	appsE [fiend', expression bs size, sigE (litE $ integerL $ fromIntegral n)
		(conT ''Int)]
	where
	fiend' = varE 'fromType
writeField _ _ (Left (Right s)) =
	appsE [varE 'fs, litE $ stringL s]
writeField bs bytes (Right v) =
	fieldValueToStr bs bytes False $ getField bs v

fieldValueToStr :: Name -> Expression -> Bool -> ExpQ -> ExpQ
fieldValueToStr bs size False =
	appE $ appE (varE 'fromType) (expression bs size)
fieldValueToStr bs size True = \val ->
	appE (varE 'cc) $ appsE [
		varE 'map, appE (varE 'fromType) (expression bs size), val]

mkReader :: String -> [BinaryStructureItem] -> DecQ
mkReader bsn body = do
	cs <- newName "cs"
	ret <- newName "ret"
	funD (mkName $ "read" ++ bsn)
		[clause [varP cs] (normalB $ mkLetRec ret $
			mkBody bsn body cs) []]

mkLetRec :: Name -> (Name -> ExpQ) -> ExpQ
mkLetRec n f = do
	rest <- newName "rest"
	letE [valD (tupP [varP n, varP rest]) (normalB $ f n) []] $
		tupE [varE n, varE rest]

mkBody :: String -> [BinaryStructureItem] -> Name -> Name -> ExpQ
mkBody bsn body cs ret = do
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
		let t = dropE' n $ varE cs'
		let p = val `equal` appE (varE 'fst)
			(appE (appE (varE 'toType) arg) $ takeE' n $ varE cs')
		let e = [e| error "bad value" |]
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
			(normalB $ appE (appE (varE 'toType) arg) $ varE cs') []
		return ([def], cs'')
	    | otherwise = error "bad"
	    where
	    n = expression ret $ bytesOf item
	    arg = expression ret $ bytesOf item

expression :: Name -> Expression -> ExpQ
expression ret e = e ret

getField :: Name -> String -> ExpQ
getField bs v = appE (varE $ mkName v) (varE bs)

equal :: Int -> ExpQ -> ExpQ
equal x y = infixE (Just $ sigE (litE $ integerL $ fromIntegral x) (conT ''Int))
	(varE '(==)) (Just y)

equal' :: String -> ExpQ -> ExpQ
equal' x y = infixE (Just $ litE $ stringL x) (varE '(==)) (Just y)

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = appE (varE 'ts) $ appsE [varE 'tk, n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'dp, n, xs]

gather :: Monad m => s -> [a] -> (a -> s -> m ([b], s)) -> m ([b], s)
gather s [] _ = return ([], s)
gather s (x : xs) f = do
	(ys, s') <- f x s
	(zs, s'') <- gather s' xs f
	return (ys ++ zs, s'')

mkInstance :: String -> DecQ
mkInstance name =
	instanceD (cxt []) (appT (conT ''RetType) (conT $ mkName name)) [
		valD (varP $ 'fromType)
			(normalB $ varE $ mkName $ "write" ++ name) [],
		valD (varP $ 'toType)
			(normalB $ varE $ mkName $ "read" ++ name) []
	 ]

mkData :: String -> [BinaryStructureItem] -> DecsQ
mkData bsn body = do
	d <- dataD (cxt []) name [] [con] [''Show]
	_ <- mkInstance bsn
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
