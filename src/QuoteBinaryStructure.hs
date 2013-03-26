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
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Char
import Data.Bits

import ParseBinaryStructure
import Classes

import qualified Data.ByteString as BS

main = do
	runQ (mkHaskellTree $ parseBinaryStructure "BinaryFileHeader") >>= print

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = mkHaskellTree . parseBinaryStructure
 }

mkHaskellTree :: BinaryStructure -> DecsQ
mkHaskellTree BinaryStructure{
	binaryStructureName = bsn,
	binaryStructureEndian = endian,
	binaryStructureBody = body } = do
		d <- mkData endian bsn body
		r <- mkReader endian bsn body
		w <- mkWriter endian bsn body
		i <- retTypeInt endian
		return $ d ++ [r, w]

mkWriter :: Endian -> String -> [BinaryStructureItem] -> DecQ
mkWriter endian bsn body = do
	bs <- newName "bs"
	let run = appE (varE 'cc) $ listE $ map
		(\bsi -> writeField endian bs (bytesOf bsi) (typeOf bsi) (sizeOf bsi)
			(valueOf endian bsi))
		body
	funD (mkName $ "write" ++ bsn)
		[clause [varP bs] (normalB run) []]

writeField :: Endian -> Name -> Expression -> Type -> Maybe Expression ->
	Either Int String -> ExpQ
writeField endian bs size (Type _) Nothing (Left n) =
	appsE [fiend, expression bs size, litE $ integerL $ fromIntegral n]
	where
	fiend = case endian of
		LittleEndian -> varE 'fii
		BigEndian -> varE 'fiiBE
writeField endian bs bytes typ size (Right v) =
	fieldValueToStr endian bs bytes (isJust size) typ $ getField bs v

fiend :: Endian -> ExpQ
fiend endian = case endian of
	LittleEndian -> varE 'fii
	BigEndian -> varE 'fiiBE

fieldValueToStr :: Endian -> Name -> Expression -> Bool -> Type -> ExpQ -> ExpQ
fieldValueToStr endian bs size False (Type typ) =
	appE $ appE (varE 'fromType) (expression bs size)
fieldValueToStr endian bs size True typ = \val -> do
	runIO $ do
		putStrLn "there"
	appE (varE 'cc) $ appsE [
		varE 'map, appE (varE 'fromType) (expression bs size), val]
	where
	addZero = appE $ correctSize' $ expression bs size
	
fieldValueToStr endian bs size bool typ = error $ show (endian, bs, size, bool, typ)

addZeros :: Int -> ExpQ
addZeros ln = do
	lst <- newName "lst"
	let bdy = infixApp (varE lst) (varE '(++)) $
		appsE [varE 'replicate, litE $ integerL $ fromIntegral ln, varE 'zero]
	lam1E (varP lst) bdy

correctSize' :: ExpQ -> ExpQ
correctSize' size = do
	lst <- newName "lst"
	let bdy = infixApp (varE lst) (varE '(++)) $
		appsE [varE 'replicate,
			infixApp size (varE '(-)) $ appE (varE 'length) $ varE lst,
			varE 'zero]
	lam1E (varP lst) bdy

correctSize :: ExpQ -> ExpQ -> ExpQ
correctSize size list = infixApp list (varE '(++)) $
	appsE [varE 'replicate,
		infixApp size (varE '(-)) $ appE (varE 'length) list,
		varE 'zero]

newNameList :: Int -> Q [Name]
newNameList 0 = return []
newNameList n = liftA2 (:) (newName "x") $ newNameList (n - 1)

mapTuple :: (Type -> ExpQ) -> [Type] -> ExpQ
mapTuple f ts = varE 'show

mkReader :: Endian -> String -> [BinaryStructureItem] -> DecQ
mkReader endian bsn body = do
	cs <- newName "cs"
	ret <- newName "ret"
	funD (mkName $ "read" ++ bsn)
		[clause [varP cs] (normalB $ mkLetRec ret $
			mkBody endian bsn body cs) []]

mkLetRec :: Name -> (Name -> ExpQ) -> ExpQ
mkLetRec n f = do
	rest <- newName "rest"
	letE [valD (tupP [varP n, varP rest]) (normalB $ f n) []] $
		tupE [varE n, varE rest]

mkBody :: Endian -> String -> [BinaryStructureItem] -> Name -> Name -> ExpQ
mkBody endian bsn body cs ret = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName "tmp"
	(defs, rest) <- gather cs body $ mkDef namePairs
	letE (map return defs) $ tupE
		[recConE (mkName bsn) (map toPair2 namePairs), varE rest]
	where
	names = rights $ map (valueOf endian) body
	toPair2 (n, nn) = return $ (mkName n, VarE nn)
	mkValD v = valD (varP v) (normalB $ litE $ integerL 45) []
	mkDef :: [(String, Name)] -> BinaryStructureItem -> Name -> Q ([Dec], Name)
	mkDef np item cs'
	    | Left val <- valueOf endian item = do
		cs'' <- newName "cs"
		let t = dropE' n $ varE cs'
		let p = val `equal` appE (varE 'fst)
			(appE tiend $ takeE' n $ varE cs')
		let e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Right var <- valueOf endian item, Just expr <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (tupP [varP $ fromJust $ lookup var np, varP cs''])
			(normalB (appsE
				[(varE 'times), expression ret expr,
					appE (varE 'toType) arg, varE cs']))
				[]
		return ([def], cs'')
	    | Right var <- valueOf endian item, Nothing <- sizeOf item,
		Type typ <- typeOf item = do
		cs'' <- newName "cs"
		def <- valD (tupP [varP $ fromJust $ lookup var np, varP cs''])
			(normalB $ appE (appE (varE 'toType) arg) $ varE cs') []
		return ([def], cs'')
	    | otherwise = error $ show $ typeOf item
	    where
	    n = expression ret $ bytesOf item
	    tiend' = varE 'toType
	    tiend = case endian of
		LittleEndian -> appE (varE 'tii) (litE $ integerL 4)
		BigEndian -> appE (varE 'tiiBE) (litE $ integerL 4)
	    arg = expression ret $ bytesOf item

strToTupple :: Int -> ExpQ
strToTupple n = (toTupple n) `dot` appE (varE 'map) (varE 'ord) `dot`
	appE (varE 'take) (litE $ integerL $ fromIntegral n)

dot :: ExpQ -> ExpQ -> ExpQ
dot f1 f2 = infixApp f1 (varE '(.)) f2

toTupple :: Int -> ExpQ
toTupple n = do
	nl <- newNameList n
	lam1E (listP $ map varP nl) (tupE $ map varE nl)

expression :: Name -> Expression -> ExpQ
expression ret (ExpressionQ e) = e ret

getField :: Name -> String -> ExpQ
getField bs v = appE (varE $ mkName v) (varE bs)

multiE :: Int -> ExpQ -> ExpQ
multiE x y = infixE (Just $ litE $ integerL $ fromIntegral x) (varE '(*)) (Just y)

multiE' :: ExpQ -> ExpQ -> ExpQ
multiE' x y = infixE (Just x) (varE '(*)) (Just y)

addE' :: ExpQ -> ExpQ -> ExpQ
addE' x y = infixE (Just x) (varE '(+)) (Just y)

divE :: ExpQ -> ExpQ -> ExpQ
divE x y = infixE (Just x) (varE 'div) (Just y)

equal :: Int -> ExpQ -> ExpQ
equal x y = infixE (Just $ litE $ integerL $ fromIntegral x) (varE '(==)) (Just y)

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = appE (varE 'ts) $ appsE [varE 'tk, n, xs]

takeE'' :: ExpQ -> ExpQ -> ExpQ
takeE'' n xs = appE (varE 'tbs) $ appsE [varE 'tk, n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'dp, n, xs]

gather :: Monad m => s -> [a] -> (a -> s -> m ([b], s)) -> m ([b], s)
gather s [] f = return ([], s)
gather s (x : xs) f = do
	(ys, s') <- f x s
	(zs, s'') <- gather s' xs f
	return $ (ys ++ zs, s'')

makeData :: BinaryStructure -> DecsQ
makeData BinaryStructure{
	binaryStructureName = bsn,
	binaryStructureEndian = endian,
	binaryStructureBody = body } = mkData endian bsn body

mkInstance :: String -> DecQ
mkInstance name =
	instanceD (cxt []) (appT (conT ''RetType) (conT $ mkName name)) [
		valD (varP $ 'fromType)
			(normalB $ varE $ mkName $ "write" ++ name) [],
		valD (varP $ 'toType)
			(normalB $ varE $ mkName $ "read" ++ name) []
	 ]

mkData :: Endian -> String -> [BinaryStructureItem] -> DecsQ
mkData endian bsn body = do
	d <- dataD (cxt []) name [] [con] [''Show]
	mkInstance bsn
	ds <- mapM makeData $ map getRepeat $ filter isRepeat body
	return $ [d] ++ concat ds
	where
	name = mkName bsn
	con = recC (mkName bsn) vsts

	vsts = flip map (filter isRight body) $ \item ->
		case (sizeOf item, typeOf item) of
			(sz, tp) -> varStrictType
				(mkName $ fromRight $ valueOf endian item) $
					strictType notStrict $
						mkType (isJust sz) tp
	isRight item
		| Right _ <- valueOf endian item = True
		| otherwise = False

mkType :: Bool -> Type -> TypeQ
mkType True t = appT listT $ mkType False t
mkType False (Type typ) = typ

appsT :: [TypeQ] -> TypeQ
appsT [t] = t
appsT (t1 : t2 : ts) = appsT (appT t1 t2 : ts)

mkTupleReader :: [Type] -> ExpQ
mkTupleReader _ = varE 'show

fromRight = either (error "not Right") id

devideN :: Int -> [a] -> [[a]]
devideN _ [] = []
devideN n xs = take n xs : devideN n (drop n xs)

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times 0 _ s = ([], s)
times n f s = let
	(ret, rest) = f s
	(rets, rest') = times (n - 1) f rest in
	(ret : rets, rest')
