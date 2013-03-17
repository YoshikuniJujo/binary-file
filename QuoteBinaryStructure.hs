{-# LANGUAGE TemplateHaskell, TupleSections #-}

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
		r <- mkReader bsn body
		w <- mkWriter bsn body
		return [d, r, w]

mkWriter :: String -> [BinaryStructureItem] -> DecQ
mkWriter bsn body = do
	bs <- newName "bs"
	let run = appE (varE 'concat) $ listE $ map
		(\bsi -> writeField bs (bytesOf bsi) (typeOf bsi) (sizeOf bsi) (valueOf bsi))
		body
	funD (mkName $ "write" ++ bsn)
		[clause [varP bs] (normalB run) []]
	where
--	body = normalB $ litE $ stringL "yet"

writeField :: Name -> Expression -> Type -> Maybe Expression -> Either Int String -> ExpQ
writeField bs size Int Nothing (Left n) =
	appsE [varE 'intToBin, expression bs size, litE $ integerL $ fromIntegral n]
writeField bs size Int Nothing (Right v) =
	appsE [varE 'intToBin, expression bs size, getField bs v]
writeField bs size Int (Just n) (Right v) = appsE [varE 'concatMap,
	appE (varE 'intToBin) (expression bs size), getField bs v]
writeField bs size String Nothing (Right v) = getField bs v

intToBin :: Int -> Int -> String
intToBin n x = intToBinGen (fromIntegral n) (fromIntegral x)

intToBinGen :: Integer -> Integer -> String
intToBinGen 0 _ = ""
intToBinGen n x = chr (fromIntegral $ x `mod` 256) :
	intToBinGen (n - 1) (x `div` 256)

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
--	let defs = map mkValD $ map snd namePairs
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
		let p = val `equal` appE (varE 'readInt) (takeE' n $ varE cs')
		let e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item, Int <- typeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ appE (varE 'readInt) $ takeE' n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE' n $ varE cs') []
		return ([def, next], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ takeE' n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE' n $ varE cs') []
		return ([def, next], cs'')
	    | Right var <- valueOf item, Just expr <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ -- listE $
				appsE [varE 'map, varE 'readInt,
				appsE [varE 'devideN, n,
--					litE $ integerL $ fromIntegral n,
			takeE' (multiE' n $ expression ret expr) $ varE cs']]) []
--		next <- valD (varP cs'') (normalB $ dropE n $ varE cs') []
		next <- valD (varP cs'') (normalB $
			dropE' (multiE' n $ expression ret expr) $ varE cs') []
--			dropE' (multiE 1 $ expression ret expr) $ varE cs') []
		return ([def, next], cs'')
{-
	    | Right var <- valueOf item, Just expr <- sizeOf item = do
		cs'' <- newName "cs"
--		def <- 
--		next <- valD (varP cs'') (normalB $
--			dropE' (multiE n $ expression ret expr) $ varE cs') []
--		return ([next], cs'')
		return ([], cs'')
-}
	    where
	    n = expression ret $ bytesOf item

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

takeE :: Int -> ExpQ -> ExpQ
takeE n xs = appsE [varE 'take, litE $ integerL $ fromIntegral n, xs]

takeE' :: ExpQ -> ExpQ -> ExpQ
takeE' n xs = appsE [varE 'take, n, xs]

dropE :: Int -> ExpQ -> ExpQ
dropE n xs = appsE [varE 'drop, litE $ integerL $ fromIntegral n, xs]

dropE' :: ExpQ -> ExpQ -> ExpQ
dropE' n xs = appsE [varE 'drop, n, xs]

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
			(Nothing, Int) -> varStrictType
				(mkName $ fromRight $ valueOf item) $
					strictType notStrict $ conT ''Int
			(_, Int) -> varStrictType
				(mkName $ fromRight $ valueOf item) $
					strictType notStrict $
						appT listT $ conT ''Int
			(Nothing, _) -> varStrictType
				(mkName $ fromRight $ valueOf item) $
					strictType notStrict $ conT ''String

	isRight item
		| Right _ <- valueOf item = True
		| otherwise = False

fromRight = either (error "not Right") id

devideN :: Int -> [a] -> [[a]]
devideN _ [] = []
devideN n xs = take n xs : devideN n (drop n xs)
