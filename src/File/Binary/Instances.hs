{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances () where

import Prelude hiding (take, drop)
import File.Binary.Classes (Field(..), Binary(..))
import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, take, drop, toChunks, fromChunks, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import qualified Data.ByteString as BS (ByteString, take, drop, concat)
import Control.Monad (replicateM)
import "monads-tf" Control.Monad.State (StateT(..), gets)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, (&&&))
import Data.Monoid (mempty)

--------------------------------------------------------------------------------

instance Field ByteString where
	type FieldArgument ByteString = Int
	fromBinary a = return . getBytes a
	toBinary _ = makeBinary

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n = return . first (BS.concat . toChunks) . getBytes n
	toBinary _ = makeBinary . fromChunks . (: [])

instance Field Char where
	type FieldArgument Char = ()
	fromBinary _ = return . first (head . BSLC.unpack) . getBytes 1
	toBinary _ = makeBinary . BSLC.pack . (: [])

instance Field Word8 where
	type FieldArgument Word8 = ()
	fromBinary _ = return . first (head . unpack) . getBytes 1
	toBinary _ = makeBinary . pack . (: [])

instance Field r => Field [r] where
	type FieldArgument [r] = (FieldArgument r, Maybe Int)
	fromBits (a, Just b) = b `times` fromBits a
	fromBits (a, Nothing) = mempty `whole` fromBits a
	consToBits (a, _) = flip $ foldr $ consToBits a

times :: Monad m => Int -> (s -> m (ret, s)) -> s -> m ([ret], s)
times n f = runStateT $ replicateM n (StateT f)

whole :: (Functor m, Monad m, Eq s) => s -> (s -> m (ret, s)) -> s -> m ([ret], s)
whole e f = runStateT $ do
	emp <- gets (== e)
	if emp then return [] else
		(:) <$> StateT f <*> (StateT $ whole e f)

--------------------------------------------------------------------------------

instance Binary String where
	getBytes n = first BSLC.pack . splitAt n
	makeBinary = BSLC.unpack

instance Binary ByteString where
	getBytes n = take (fromIntegral n) &&& drop (fromIntegral n)
	makeBinary = id

instance Binary BS.ByteString where
	getBytes n = fromChunks . (: []) . BS.take n &&& BS.drop n
	makeBinary = BS.concat . toChunks
