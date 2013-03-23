{-# LANGUAGE
	TemplateHaskell,
	TupleSections,
	PatternGuards,
	TypeSynonymInstances,
	FlexibleInstances #-}

module QuoteBinaryStructure (
	binary
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
	binaryStructureBody = body } = do
		d <- mkData bsn body
--		dbg <- debugReadType
		r <- mkReader bsn body
		w <- mkWriter bsn body
		return [d, r, w]

mkWriter :: String -> [BinaryStructureItem] -> DecQ
mkWriter bsn body = do
	bs <- newName "bs"
	let run = appE (varE 'cc) $ listE $ map
		(\bsi -> writeField bs (bytesOf bsi) (typeOf bsi) (sizeOf bsi) (valueOf bsi))
		body
	funD (mkName $ "write" ++ bsn)
		[clause [varP bs] (normalB run) []]

writeField :: Name -> Expression -> Type -> Maybe Expression ->
	Either Int String -> ExpQ
writeField bs size Int Nothing (Left n) =
	appsE [varE 'fi, expression bs size, litE $ integerL $ fromIntegral n]
writeField bs bytes typ size (Right v) =
	fieldValueToStr bs bytes (isJust size) typ $ getField bs v

fieldValueToStr :: Name -> Expression -> Bool -> Type -> ExpQ -> ExpQ
fieldValueToStr bs size False Int = appE $ appE (varE 'fi) (expression bs size)
fieldValueToStr bs size True Int = appE (varE 'cc) . appE (appsE [varE 'map,
	appE (varE 'fi) (expression bs size)])
fieldValueToStr bs size False String = appE $ varE 'fs
fieldValueToStr bs size False ByteString = appE $ varE 'fbs
fieldValueToStr bs size False (Tuple ts) = \val -> do
	nl <- newNameList $ length ts
	let	def = valD (tupP $ map varP nl) (normalB val) []
		bdy = zipWith (fieldValueToStr bs (Number 1) False) ts $ map varE nl
	 in	letE [def] $ appE (varE 'cc) $ listE bdy
fieldValueToStr bs size True (Tuple ts) = \val -> do
	runIO $ do
		putStrLn "here"
		print ts
	nl <- newNameList $ length ts
	let	ps = tupP $ map varP nl
		bdy = zipWith (fieldValueToStr bs (Number 1) False) ts $ map varE nl
	 in	appE (varE 'cc) $ appsE [
			varE 'map,
			lamE [ps] $ appE (varE 'cc) $ addZero $ listE bdy,
			val]
	where
	addZero = -- appE $ addZeros 1
		appE $ correctSize' $ expression bs size
{-
	let	ps = tupP $ map varP nl
		bdy = zipWith (fieldValueToStr bs (Number 1) False) ts $ map varE nl
	 in	appE (varE 'cc) $ appE (correctSize' $ expression bs size) $ appsE [
			varE 'map, lamE [ps] $ appE (varE 'cc) $ listE bdy, val]
-}
{-
	 in	appE (varE 'cc) $ correctSize (expression bs size) $ appsE [
			varE 'map, lamE [ps] $ appE (varE 'cc) $ listE bdy, val]
-}

addZeros :: Int -> ExpQ
addZeros ln = do
	lst <- newName "lst"
	let bdy = infixApp (varE lst) (varE '(++)) $
		appsE [varE 'replicate, litE $ integerL $ fromIntegral ln, varE 'zero]
	lam1E (varP lst) bdy

correctSize' :: ExpQ -> ExpQ
correctSize' size = do
--	let size = litE $ IntegerL 100
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

intToBin :: Int -> Int -> String
intToBin n x = intToBinGen (fromIntegral n) (fromIntegral x)

intToBinGen :: Integer -> Integer -> String
intToBinGen 0 _ = ""
intToBinGen n x = chr (fromIntegral $ x `mod` 256) :
	intToBinGen (n - 1) (x `div` 256)

-- readerType :: String -> DecQ
-- readerType = sigD (mkName $ "read" ++ bsn) [t| Str a => a ->

-- debugReadType :: DecQ = [d| readBitmap :: Str a => a -> Bitmap |]

mkReader :: String -> [BinaryStructureItem] -> DecQ
mkReader bsn body = do
	cs <- newName "cs"
	ret <- newName "ret"
	funD (mkName $ "read" ++ bsn)
		[clause [varP cs] (normalB $ mkLetRec ret $ mkBody bsn body cs) []]

mkLetRec :: Name -> (Name -> ExpQ) -> ExpQ
mkLetRec n f = letE [valD (varP n) (normalB $ f n) []] $ varE n

mkBody :: String -> [BinaryStructureItem] -> Name -> Name -> ExpQ
mkBody bsn body cs ret = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName "tmp"
	defs <- gather cs body $ mkDef namePairs
	letE (map return defs) $ recConE (mkName bsn) (map toPair2 namePairs)
	where
	names = rights $ map valueOf body
	toPair2 (n, nn) = return $ (mkName n, VarE nn)
	mkValD v = valD (varP v) (normalB $ litE $ integerL 45) []
	mkDef :: [(String, Name)] -> BinaryStructureItem -> Name -> Q ([Dec], Name)
	mkDef np item cs'
	    | Left val <- valueOf item = do
		cs'' <- newName "cs"
		let t = dropE' n $ varE cs'
		let p = val `equal` appE (varE 'ti) (takeE' n $ varE cs')
		let e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item, Int <- typeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ appE (varE 'ti) $ takeE' n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE' n $ varE cs') []
		return ([def, next], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item, ByteString <- typeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ takeE'' n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE' n $ varE cs') []
		return ([def, next], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ takeE' n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE' n $ varE cs') []
		return ([def, next], cs'')
	    | Right var <- valueOf item, Just expr <- sizeOf item, Tuple ts <- typeOf item =
		if all (== Int) ts then do
			cs'' <- newName "cs"
			def <- valD (varP $ fromJust $ lookup var np)
				(normalB $
					appsE [varE 'map, strToTupple $ length ts,
					appsE [varE 'devideN, n,
				takeE' (multiE' n $ expression ret expr) $ varE cs']]) []
			next <- valD (varP cs'') (normalB $
				dropE' (multiE' n $ expression ret expr) $ varE cs') []
			return ([def, next], cs'')
		    else error "hogeru"
	    | Right var <- valueOf item, Just expr <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $
				appsE [varE 'map, varE 'ti,
				appsE [varE 'devideN, n,
			takeE' (multiE' n $ expression ret expr) $ varE cs']]) []
		next <- valD (varP cs'') (normalB $
			dropE' (multiE' n $ expression ret expr) $ varE cs') []
		return ([def, next], cs'')
	    | otherwise = error $ show $ typeOf item
	    where
	    n = expression ret $ bytesOf item

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
expression ret (Variable v) = appE (varE $ mkName v) (varE ret)
expression _ (Number n) = litE $ integerL $ fromIntegral n
expression ret (Division x y) = divE (expression ret x) (expression ret y)
expression ret (Multiple x y) = multiE' (expression ret x) (expression ret y)

getField :: Name -> String -> ExpQ
getField bs v = appE (varE $ mkName v) (varE bs)

multiE :: Int -> ExpQ -> ExpQ
multiE x y = infixE (Just $ litE $ integerL $ fromIntegral x) (varE '(*)) (Just y)

multiE' :: ExpQ -> ExpQ -> ExpQ
multiE' x y = infixE (Just x) (varE '(*)) (Just y)

divE :: ExpQ -> ExpQ -> ExpQ
divE x y = infixE (Just x) (varE 'div) (Just y)

equal :: Int -> ExpQ -> ExpQ
equal x y = infixE (Just $ litE $ integerL $ fromIntegral x) (varE '(==)) (Just y)

-- takeE :: Int -> ExpQ -> ExpQ
-- takeE n xs = appsE [varE 'tk, litE $ integerL $ fromIntegral n, xs]

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = appE (varE 'ts) $ appsE [varE 'tk, n, xs]

takeE'' :: ExpQ -> ExpQ -> ExpQ
takeE'' n xs = appE (varE 'tbs) $ appsE [varE 'tk, n, xs]

-- dropE :: Int -> ExpQ -> ExpQ
-- dropE n xs = appsE [varE 'dp, litE $ integerL $ fromIntegral n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'dp, n, xs]

gather :: Monad m => s -> [a] -> (a -> s -> m ([b], s)) -> m [b]
gather s [] f = return []
gather s (x : xs) f = do
	(ys, s') <- f x s
	zs <- gather s' xs f
	return $ ys ++ zs

mkData :: String -> [BinaryStructureItem] -> DecQ
mkData bsn body =
	dataD (cxt []) name [] [con] [''Show]
	where
	name = mkName bsn
	con = recC (mkName bsn) vsts

	vsts = flip map (filter isRight body) $ \item ->
		case (sizeOf item, typeOf item) of
			(sz, tp) -> varStrictType
				(mkName $ fromRight $ valueOf item) $
					strictType notStrict $ mkType (isJust sz) tp -- conT ''Int
	isRight item
		| Right _ <- valueOf item = True
		| otherwise = False

mkType :: Bool -> Type -> TypeQ
mkType True t = appT listT $ mkType False t
mkType False Int = conT ''Int
mkType False String = conT ''String
mkType False ByteString = conT ''BS.ByteString
mkType False (Tuple ts) = appsT $ tupleT (length ts) : map (mkType False) ts

appsT :: [TypeQ] -> TypeQ
appsT [t] = t
appsT (t1 : t2 : ts) = appsT (appT t1 t2 : ts)

mkTupleReader :: [Type] -> ExpQ
mkTupleReader _ = varE 'show

fromRight = either (error "not Right") id

devideN :: Int -> [a] -> [[a]]
devideN _ [] = []
devideN n xs = take n xs : devideN n (drop n xs)

class Str a where
	tk :: Int -> a -> a
	dp :: Int -> a -> a
	ts :: a -> String
	fs :: String -> a
	fbs :: BS.ByteString -> a
	tbs :: a -> BS.ByteString
	ti :: a -> Int
	fi :: Int -> Int -> a
	cc :: [a] -> a
	zero :: a

instance Str String where
	tk = take
	dp = drop
	ts = id
	fs = id
	fbs = ts
	tbs = fs
	ti = readInt
	fi = intToBin
	cc = concat
	zero = "\0"

instance Str BS.ByteString where
	tk = BS.take
	dp = BS.drop
	ts = map (chr . fromIntegral) . BS.unpack
	fs = BS.pack . map (fromIntegral . ord)
	fbs = id
	tbs = id
	ti = readInt . map (chr . fromIntegral) . BS.unpack
	fi n = BS.pack . map (fromIntegral . ord) . intToBin n
	cc = BS.concat
	zero = BS.singleton 0
