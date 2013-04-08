{-# LANGUAGE TypeFamilies #-}

module File.Binary.Classes (Field(..), Binary(..), BitsBinary) where

import Data.Monoid
import Data.ByteString.Lazy (ByteString)

type BitsBinary b = ([Bool], b)

class Field r where
	type FieldArgument r
	fromBinary :: Binary s => FieldArgument r -> s -> (r, s)
	toBinary :: Binary s => FieldArgument r -> r -> s
	fromBitsBinary :: Binary s =>
		FieldArgument r -> BitsBinary s -> (r, BitsBinary s)
	consToBitsBinary :: Binary s =>
		FieldArgument r -> r -> BitsBinary s -> BitsBinary s

	fromBitsBinary a ([], b) = let (f, rest) = fromBinary a b in
		(f, ([], rest))
	fromBitsBinary _ _ = error "fromBitsBinary: not 8 bits"
	consToBitsBinary a f ([], b) = ([], toBinary a f `mappend` b)
	consToBitsBinary _ _ _ = error "consToBitsBinary: not 8 bits"

	fromBinary a b = case fromBitsBinary a ([], b) of
		(f, ([], rest)) -> (f, rest)
		_ -> error "fromBinary: not 8 bits"
	toBinary a f = case consToBitsBinary a f ([], mempty) of
		([], bin) -> bin
		_ -> error "toBinary: not 8 bits"

class (Eq b, Monoid b) => Binary b where
	getBytes :: Int -> b -> (ByteString, b)
	makeBinary :: ByteString -> b
