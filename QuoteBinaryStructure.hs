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

mkReader :: String -> [(Int, Either Int String)] -> DecQ
mkReader bsn body = do
	cs <- newName "cs"
	funD (mkName $ "read" ++ bsn)
		[clause [varP cs] (normalB $ mkBody bsn body cs) []]

mkBody :: String -> [(Int, Either Int String)] -> Name -> ExpQ
mkBody bsn body cs = do
	namePairs <- for names $ \n -> return . (n ,) =<< newName n
--	let defs = map mkValD $ map snd namePairs
	defs <- gather cs body $ mkDef namePairs
	letE (map return defs) $ recConE (mkName bsn) (map toPair2 namePairs)
	where
	names = rights $ map snd body
	toPair2 (n, nn) = return $ (mkName n, VarE nn)
	mkValD v = valD (varP v) (normalB $ litE $ integerL 45) []
	mkDef :: [(String, Name)] -> (Int, Either Int String) -> Name -> Q ([Dec], Name)
	mkDef np (n, Left val) cs' = do
		cs'' <- newName "cs"
		d <- valD (varP cs'') (normalB $ appsE
			[varE 'drop, litE $ integerL $ fromIntegral n, varE cs']) []
		return ([d], cs'')
	mkDef np (n, Right var) cs' = do
		cs'' <- newName "cs"
		def <- valD (varP $ fromJust $ lookup var np)
			(normalB $ appE (varE 'readInt) $ appsE
			[varE 'take, litE $ integerL $ fromIntegral n, varE cs']) []
		next <- valD (varP cs'') (normalB $ appsE
			[varE 'drop, litE $ integerL $ fromIntegral n, varE cs']) []
		return ([def, next], cs'')

readInt :: String -> Int
readInt "" = 0
readInt (c : cs) = ord c + 2 ^ 8 * readInt cs

gather :: Monad m => s -> [a] -> (a -> s -> m ([b], s)) -> m [b]
gather s [] f = return []
gather s (x : xs) f = do
	(ys, s') <- f x s
	zs <- gather s' xs f
	return $ ys ++ zs

mkData bsn body =
--	sequence [dataD (cxt []) name [] [con] [''Show]]
	dataD (cxt []) name [] [con] [''Show]
	where
	name = mkName bsn
	con = recC (mkName bsn) $ map toVST names
	names = rights $ map snd body
	toVST n = varStrictType (mkName n) $
		strictType notStrict $ conT ''Int
