{-# LANGUAGE TemplateHaskell #-}

module QuoteBinaryStructure (
	binary
) where

import Prelude hiding (sequence)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Traversable

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
	binaryStructureBody = body } =
	sequence [dataD (cxt []) name [] [con] [''Show]]
	where
	name = mkName bsn
	con = normalC (mkName bsn) []
