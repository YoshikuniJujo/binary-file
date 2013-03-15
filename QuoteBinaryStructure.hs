{-# LANGUAGE TemplateHaskell, TupleSections #-}

module QuoteBinaryStructure (
	binary
) where

import Prelude hiding (sequence)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Traversable hiding (mapM)
import Data.Either
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Char

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
		return [d, r]

mkReader :: String -> [BinaryStructureItem] -> DecQ
mkReader bsn body = do
	cs <- newName "cs"
	funD (mkName $ "read" ++ bsn)
		[clause [varP cs] (normalB $ mkBody bsn body cs) []]

mkBody :: String -> [BinaryStructureItem] -> Name -> ExpQ
mkBody bsn body cs = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName n
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
		let t = dropE n $ varE cs'
		let p = val `equal` appE (varE 'readInt) (takeE n $ varE cs')
		let e = [e| error "bad value" |]
		d <- valD (varP cs'') (normalB $ condE p t e) []
		return ([d], cs'')
	    | Right var <- valueOf item, Nothing <- sizeOf item = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ appE (varE 'readInt) $ takeE n $ varE cs') []
		next <- valD (varP cs'') (normalB $ dropE n $ varE cs') []
		return ([def, next], cs'')
--	    | Right var <- valueOf item, Just (Left val) <- sizeOf item = do
--		css'' <- newName "cs"
	    where
	    n = bytesOf item

equal :: Int -> ExpQ -> ExpQ
equal x y = infixE (Just $ litE $ integerL $ fromIntegral x) (varE '(==)) (Just y)

takeE :: Int -> ExpQ -> ExpQ
takeE n xs = appsE [varE 'take, litE $ integerL $ fromIntegral n, xs]

dropE :: Int -> ExpQ -> ExpQ
dropE n xs = appsE [varE 'drop, litE $ integerL $ fromIntegral n, xs]

readInt :: String -> Int
readInt "" = 0
readInt (c : cs) = ord c + 2 ^ 8 * readInt cs

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

	vsts = map toVST names
	names = rights $ map valueOf body
	toVST n = varStrictType (mkName n) $
		strictType notStrict $ conT ''Int
