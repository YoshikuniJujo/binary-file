import BitmapSimple
import Data.List
import Data.Maybe

getLena :: IO Image
getLena = readBMPImage "tmp/lena.bmp"

getSample :: IO Image
getSample = readBMPImage "tmp/out/sample.bmp"

ccTable :: [(Char, (Int, Int, Int))]
ccTable = [
	(' ', (255, 255, 255)),
	('#', (0, 0, 0)),
	('$', (255, 0, 0)),
	('*', (255, 127, 127)),
	('+', (255, 223, 223))
--	('+', (255, 255, 255))
 ]

colorTable :: [(Int, Int, Int)]
colorTable = map snd ccTable

strToImageBody :: [String] -> [[Int]]
strToImageBody = map $ map (fromJust . flip elemIndex (map fst ccTable))

strToImage :: [String] -> Image
strToImage strs = Image{ pallet = colorTable, body = strToImageBody strs }

horse :: [String]
horse = [
	"       ##   ",
	"     ##  #  ",
	"   ##     # ",
	"  #     $ # ",
	"  #   #   # ",
	"  #   ### # ",
	" #    #  ## ",
	"  #   #  ## ",
	"   #   #    ",
	"    #   #   ",
	"     #  #   ",
	"      # #   ",
	"       ##   "
 ]

bigHorse :: [String]
bigHorse = [
	"       ##   ",
	"       ##   ",
	"     ##  #  ",
	"     ##  #  ",
	"   ##     # ",
	"   ##     # ",
	"  #     $ # ",
	"  #     $ # ",
	"  #   #   # ",
	"  #   #   # ",
	"  #   ### # ",
	"  #   ### # ",
	" #    #  ## ",
	" #    #  ## ",
	"  #   #  ## ",
	"  #   #  ## ",
	"   #   #    ",
	"   #   #    ",
	"    #   #   ",
	"    #   #   ",
	"     #  #   ",
	"     #  #   ",
	"      # #   ",
	"      # #   ",
	"       ##   ",
	"       ##   "
 ]

circle :: [String]
circle = map
	(\y -> map
		(\x -> if x ^ (2 :: Int) + y ^ (2 :: Int) < 6400 then '$' else
			if x ^ (2 :: Int) + y ^ (2 :: Int) < 6561 then '*' else
			if x ^ (2 :: Int) + y ^ (2 :: Int) < 6724 then '+' else ' ')
		[-160 .. 160])
	[-120 .. 120 :: Int]
