{-# LANGUAGE
	TypeFamilies,
	TypeSynonymInstances,
	FlexibleInstances,
	PackageImports,
	OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances () where

import Prelude hiding (take, drop, span)
import File.Binary.Classes (Field(..), Binary(..))
import Data.Word (Word8)
import Data.ByteString.Lazy
	(ByteString, take, drop, toChunks, fromChunks, pack, unpack, uncons, span)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import qualified Data.ByteString as BS (ByteString, take, drop, concat, uncons, span)
import qualified Data.ByteString.Char8 ()
import Control.Monad (foldM)
import "monads-tf" Control.Monad.State (StateT(..))
import Control.Applicative ((<$>))
import Control.Arrow (first, (&&&))
import Data.Monoid (mempty)
import Data.Char
import Data.Maybe

--------------------------------------------------------------------------------

instance Field ByteString where
	type FieldArgument ByteString = Int
	fromBinary a = return . getBytes a
	toBinary _ = return . makeBinary

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n = return . first (BS.concat . toChunks) . getBytes n
	toBinary _ = return . makeBinary . fromChunks . (: [])

instance Field Char where
	type FieldArgument Char = ()
	fromBinary _ = return . first (head . BSLC.unpack) . getBytes 1
	toBinary _ = return . makeBinary . BSLC.pack . (: [])

instance Field Word8 where
	type FieldArgument Word8 = ()
	fromBinary _ = return . first (head . unpack) . getBytes 1
	toBinary _ = return . makeBinary . pack . (: [])

instance Field r => Field [r] where
	type FieldArgument [r] = [FieldArgument r]
	fromBits as = smap mempty as fromBits
	consToBits as fs ret = foldM (flip $ uncurry consToBits) ret $ reverse $ zip as fs

myMapM :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m [b]
myMapM _ [] = return []
myMapM f (x : xs) = do
	ret <- f x
	case ret of
		Just y -> (y :) <$> myMapM f xs
		Nothing -> return []

smap :: (Monad m, Functor m, Eq s) =>
	s -> [a] -> (a -> s -> m (ret, s)) -> s -> m ([ret], s)
smap e xs f = runStateT $ myMapM (StateT . f') xs
	where
	f' x s	| s == e = return (Nothing, s)
		| otherwise = first Just <$> f x s

--------------------------------------------------------------------------------

instance Binary String where
	getBytes n = first BSLC.pack . splitAt n
	unconsByte = fromIntegral . ord . head &&& tail
	makeBinary = BSLC.unpack

instance Binary ByteString where
	getBytes n = take (fromIntegral n) &&& drop (fromIntegral n)
	spanBytes = span
	unconsByte = fromMaybe (0, "") . uncons
	makeBinary = id

instance Binary BS.ByteString where
	getBytes n = fromChunks . (: []) . BS.take n &&& BS.drop n
	spanBytes p = first (fromChunks . (: [])) . BS.span p
	unconsByte = fromMaybe (0, "") . BS.uncons
	makeBinary = BS.concat . toChunks
