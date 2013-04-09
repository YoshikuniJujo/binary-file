{-# LANGUAGE
	TypeFamilies,
	TypeSynonymInstances,
	FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances () where

import File.Binary.Classes (Field(..), Binary(..))
import qualified Data.ByteString as BS
	(ByteString, take, drop, concat)
import qualified Data.ByteString.Lazy as BSL
	(ByteString, take, drop, toChunks, fromChunks)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import Control.Arrow (first, (&&&))
import Data.Monoid

instance Field BSL.ByteString where
	type FieldArgument BSL.ByteString = Int
	fromBinary = getBytes
	toBinary _ = makeBinary

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n str =
		first (BS.concat . BSL.toChunks) $ getBytes n str
	toBinary _ = makeBinary . BSL.fromChunks . (: [])

instance Field Char where
	type FieldArgument Char = ()
	fromBinary _ str = (head $ BSLC.unpack t, d)
		where
		(t, d) = getBytes 1 str
	toBinary _ = makeBinary . BSLC.pack . (: [])

instance Field r => Field [r] where
	type FieldArgument [r] = (FieldArgument r, Maybe Int)
	fromBits (a, Just b) s = (b `times` fromBits a) s
	fromBits (a, Nothing) s = whole mempty (fromBits a) s
	consToBits (a, _) rs bin = foldr (consToBits a) bin rs

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times 0 _ s = ([], s)
times n f s = let
	(ret, rest) = f s
	(rets, rest') = times (n - 1) f rest in
	(ret : rets, rest')

whole :: Eq s => s -> (s -> (ret, s)) -> s -> ([ret], s)
whole emp f s
	| s == emp = ([], s)
	| otherwise = let
		(ret, rest) = f s
		(rets, rest') = whole emp f rest in
		(ret : rets, rest')

--------------------------------------------------------------------------------

instance Binary String where
	getBytes n = BSLC.pack . take n &&& drop n
	makeBinary = BSLC.unpack

instance Binary BSL.ByteString where
	getBytes n = BSL.take (fromIntegral n) &&& BSL.drop (fromIntegral n)
	makeBinary = id

{-
	appendBinary = BSL.append
	concatBinary = BSL.concat
	emptyBinary = (== 0) . BSL.length
-}

instance Binary BS.ByteString where
	getBytes n = BSL.fromChunks . (: []) . BS.take n &&& BS.drop n
	makeBinary = BS.concat . BSL.toChunks

{-
	appendBinary = BS.append
	concatBinary = BS.concat
	emptyBinary = (== 0) . BS.length
-}
