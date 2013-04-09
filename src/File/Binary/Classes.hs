{-# LANGUAGE TypeFamilies, TupleSections #-}

module File.Binary.Classes (Field(..), Binary(..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid (Monoid, mappend, mempty)
import Control.Arrow (second)

type AddBits b = ([Bool], b)

class Field f where
	type FieldArgument f
	fromBinary :: Binary b => FieldArgument f -> b -> (f, b)
	toBinary :: Binary b => FieldArgument f -> f -> b
	fromBits :: Binary b => FieldArgument f -> AddBits b -> (f, AddBits b)
	consToBits :: Binary b => FieldArgument f -> f -> AddBits b -> AddBits b

	fromBits a ([], b) = second ([] ,) $ fromBinary a b
	fromBits _ _ = error "fromBits: not bytes (1 byte = 8 bits)"
	consToBits a f ([], b) = ([], toBinary a f `mappend` b)
	consToBits _ _ _ = error "consToBits: not bytes (1 byte = 8 bits)"

	fromBinary a b = case fromBits a ([], b) of
		(f, ([], rest)) -> (f, rest)
		_ -> error "fromBinary: not bytes (1 byte = 8 bits)"
	toBinary a f = case consToBits a f ([], mempty) of
		([], bin) -> bin
		_ -> error "toBinary: not bytes (1 byte = 8 bits)"

class (Eq b, Monoid b) => Binary b where
	getBytes :: Int -> b -> (ByteString, b)
	makeBinary :: ByteString -> b
