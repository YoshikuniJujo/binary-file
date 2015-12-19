{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings,
	TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.ByteString.Char8 as BS

class ListLike t where
	type Element t
	hd :: t -> Element t
	tl :: t -> t
	cons :: Element t -> t -> t
	nl :: t -> Bool

elemLL :: (ListLike t, Eq (Element t)) => Element t -> t -> Bool
elemLL e l
	| nl l = False
	| e == hd l = True
	| otherwise = elemLL e $ tl l

instance ListLike BS.ByteString where
	type Element BS.ByteString = Char
	hd = BS.head
	tl = BS.tail
	cons = BS.cons
	nl = BS.null

instance ListLike String where
	type Element String = Char
	hd = head
	tl = tail
	cons = (:)
	nl = null
