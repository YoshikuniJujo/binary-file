{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module ParseBinaryStructure (
	BinaryStructure(..),
	parseBinaryStructure
) where

import Text.Peggy

main :: IO ()
main = do
	putStrLn "ParseBinaryStructure"

data BinaryStructure = BinaryStructure {
	binaryStructureName :: String,
	binaryStructureBody :: [(Int, Either Int String)]
 } deriving Show

parseBinaryStructure :: String -> BinaryStructure
parseBinaryStructure src = case parseString top "<code>" src of
	Right bs -> bs
	Left ps -> error $ show ps

[peggy|

top :: BinaryStructure
	= spaces name	{ BinaryStructure $2 undefined }

spaces :: ()
	= [ \n]*		{ () }

name :: String
	= [A-Z][a-zA-Z0-9]*	{ $1 : $2 }

|]
