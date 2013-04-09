{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.Instances () where

import Prelude hiding (take, drop)
import File.Binary.Classes (Field(..), Binary(..))
import Data.ByteString.Lazy (ByteString, take, drop, toChunks, fromChunks)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString as BS (ByteString, take, drop, concat)
import Control.Monad (replicateM)
import "monads-tf" Control.Monad.State (StateT(..), runState, gets)
import "monads-tf" Control.Monad.Identity (Identity(..))
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, (&&&))
import Data.Monoid (mempty)

--------------------------------------------------------------------------------

instance Field ByteString where
	type FieldArgument ByteString = Int
	fromBinary = getBytes
	toBinary _ = makeBinary

instance Field BS.ByteString where
	type FieldArgument BS.ByteString = Int
	fromBinary n = first (BS.concat . toChunks) . getBytes n
	toBinary _ = makeBinary . fromChunks . (: [])

instance Field Char where
	type FieldArgument Char = ()
	fromBinary _ = first (head . unpack) . getBytes 1
	toBinary _ = makeBinary . pack . (: [])

instance Field r => Field [r] where
	type FieldArgument [r] = (FieldArgument r, Maybe Int)
	fromBits (a, Just b) = b `times` fromBits a
	fromBits (a, Nothing) = mempty `whole` fromBits a
	consToBits (a, _) = flip $ foldr $ consToBits a

times :: Int -> (s -> (ret, s)) -> s -> ([ret], s)
times n f = runState $ replicateM n (StateT $ Identity . f)

whole :: Eq s => s -> (s -> (ret, s)) -> s -> ([ret], s)
whole e f = runState $ do
	emp <- gets (== e)
	if emp then return [] else
		(:) <$> (StateT $ Identity . f) <*> (StateT $ Identity . whole e f)

--------------------------------------------------------------------------------

instance Binary String where
	getBytes n = first pack . splitAt n
	makeBinary = unpack

instance Binary ByteString where
	getBytes n = take (fromIntegral n) &&& drop (fromIntegral n)
	makeBinary = id

instance Binary BS.ByteString where
	getBytes n = fromChunks . (: []) . BS.take n &&& BS.drop n
	makeBinary = BS.concat . toChunks
