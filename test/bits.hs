import Data.Word
import Data.Bits

type BitBinary = (BitString, [Word8])

data BitString = BitString {
	bitIndex :: Int,
	bigEnd :: [Bool],
	littleEnd :: [Bool] } deriving Show

emptyBitString = BitString 0 [] []

getLittle, getBig :: Int -> BitBinary -> ([Bool], BitBinary)

getLittle 0 bb = ([], bb)
getLittle n (BitString { bitIndex = _, bigEnd = [], littleEnd = [] }, w : ws) =
	getLittle n (BitString {
		bitIndex = 0,
		bigEnd = [],
		littleEnd = wordToBoolsLittle w }, ws)
getLittle n (BitString { bitIndex = i, bigEnd = be, littleEnd = [] }, ws) =
	getLittle n (BitString {
		bitIndex = i,
		bigEnd = [],
		littleEnd = reverse be }, ws)
getLittle n (BitString { bitIndex = i, bigEnd = be, littleEnd = l : le }, ws) = let
	(r, rest) = getLittle (n - 1) (BitString {
		bitIndex = i + 1,
		bigEnd = be,
		littleEnd = le }, ws) in
	(l : r, rest)

getBig 0 bb = ([], bb)
getBig n (BitString { bitIndex = _, bigEnd = [], littleEnd = [] }, w : ws) =
	getBig n (BitString {
		bitIndex = 0,
		bigEnd = wordToBoolsBig w,
		littleEnd = [] }, ws)
getBig n (BitString { bitIndex = i, bigEnd = [], littleEnd = le }, ws) =
	getBig n (BitString {
		bitIndex = i,
		bigEnd = reverse le,
		littleEnd = [] }, ws)
getBig n (BitString { bitIndex = i, bigEnd = b : be, littleEnd = le }, ws) = let
	(r, rest) = getBig (n - 1) (BitString {
		bitIndex = i,
		bigEnd = be,
		littleEnd = le }, ws) in
	(b : r, rest)

consLittle :: [Bool] -> BitBinary -> BitBinary
consLittle [] bb = bb
consLittle ba@(b : bs)
	bb@(BitString { bitIndex = i, bigEnd = be, littleEnd = le }, ws)
	| i == 0 =
		consLittle ba (emptyBitString,
			boolsToWordLittle (le ++ reverse be) : ws)
	| otherwise = let
		(BitString { bitIndex = i', bigEnd = be', littleEnd = le' }, ws') =
			consLittle bs bb in
		(BitString { bitIndex = i' - 1, bigEnd = be', littleEnd = b : le' }, ws')

consBig :: [Bool] -> BitBinary -> BitBinary
consBig [] bb = bb
consBig ba@(b : bs)
	bb@(BitString { bitIndex = i, bigEnd = be, littleEnd = le }, ws)
	| i + length be + length le == 8 =
		consBig bs (emptyBitString,
			boolsToWordBig (be ++ reverse le) : ws)
	| otherwise = let
		(BitString { bitIndex = i', bigEnd = be', littleEnd = le' }, ws') =
			consBig bs bb in
		(BitString { bitIndex = i', bigEnd = b : be', littleEnd = le'},
			ws')

wordToBoolsLittle :: Word8 -> [Bool]
wordToBoolsLittle = wtbl 8
	where
	wtbl 0 _ = []
	wtbl n w = toEnumW (1 .&. w) : wtbl (n - 1) (w `shiftR` 1)

wordToBoolsBig :: Word8 -> [Bool]
wordToBoolsBig = wtbb [] 8
	where
	wtbb :: [Bool] -> Int -> Word8 -> [Bool]
	wtbb r 0 _ = r
	wtbb r n w = wtbb (toEnumW (1 .&. w) : r) (n - 1) (w `shiftR` 1)

boolsToWordLittle :: [Bool] -> Word8
boolsToWordLittle [] = 0
boolsToWordLittle (b : bs) = fromEnumW b .|. (boolsToWordLittle bs) `shiftL` 1

boolsToWordBig :: [Bool] -> Word8
boolsToWordBig = btwb 0 8
	where
	btwb r n [] = r `shiftL` n
	btwb r n (b : bs) = btwb (r `shiftL` 1 .|. fromEnumW b) (n - 1) bs

toEnumW = toEnum . fromIntegral
fromEnumW = fromIntegral . fromEnum
