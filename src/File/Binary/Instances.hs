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
	fromBinary n str = getBytes n str
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
	fromBinary (a, Just b) s = (b `times` fromBinary a) s
	fromBinary (a, Nothing) s = whole (fromBinary a) s
	toBinary (a, _) rs = mconcat $ map (toBinary a) rs

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times 0 _ s = ([], s)
times n f s = let
	(ret, rest) = f s
	(rets, rest') = times (n - 1) f rest in
	(ret : rets, rest')

whole :: Binary s => (s -> (ret, s)) -> s -> ([ret], s)
whole f s
	| s == mempty = ([], s)
	| otherwise = let
		(ret, rest) = f s
		(rets, rest') = whole f rest in
		(ret : rets, rest')

--------------------------------------------------------------------------------

instance Binary String where
	getBytes n = BSLC.pack . take n &&& drop n
	makeBinary = BSLC.unpack

{-
	appendBinary = (++)
	emptyBinary = null
-}

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
