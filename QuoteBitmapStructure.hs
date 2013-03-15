{-# LANGUAGE TemplateHaskell #-}

module QuoteBitmapStructure (
	binary
) where

import Prelude hiding (sequence)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Traversable

import ParseBinaryStructure

main = do
	runQ (mkHaskellTree $ parseBitmapStructure "BitmapFileHeader") >>= print

binary :: QuasiQuoter
binary = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = mkHaskellTree . parseBitmapStructure
 }

mkHaskellTree :: BitmapStructure -> DecsQ
mkHaskellTree BitmapStructure{
	bitmapStructureName = bsn,
	bitmapStructureBody = body } =
	sequence [dataD (cxt []) name [] [con] [''Show]]
	where
	name = mkName bsn
	con = normalC (mkName bsn) []
