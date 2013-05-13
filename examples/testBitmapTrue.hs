import BitmapTrue
import Control.Arrow
import Data.List

r :: Double
r = 100

circleXs :: [Double]
circleXs = map (\y -> sqrt (r ^ (2 :: Int) - y ^ (2 :: Int)))
	[0 .. sqrt (r ^ (2 :: Int) / 2)]

getPair :: Double -> (Int,(Double, Double))
getPair x = (f, (1 - y, y))
	where
	(f, y) = properFraction x

getPair' :: Double -> (Int,(Double, Double))
getPair' x = (f, (1, 0))
	where
	(f, _) = properFraction x

getPair'' :: Double -> (Int,(Double, Double))
getPair'' x = (f, (1, 1))
	where
	(f, _) = properFraction x

getPair''' :: Double -> (Int,(Double, Double))
getPair''' x = (f, (0, 1))
	where
	(f, _) = properFraction x

getPair'''' :: Double -> (Int,(Double, Double))
getPair'''' x
	| y > 0.5 = (f, (0, 1))
	| otherwise = (f, (1, 0))
	where
	(f, y) = properFraction x

circlePairs :: [(Int, (Double, Double))]
circlePairs = map getPair circleXs

getPixels :: Int -> (Int, (Double, Double)) -> [((Int, Int), Int)]
getPixels y (x, (p1, p2)) =
	[((x, y), floor $ 255 * p1), ((x + 1, y), floor $ 255 * p2)]

circle8 :: [((Int, Int), Int)]
circle8 = concat $ zipWith getPixels [0 .. ] circlePairs

mkLine :: (Int, (Double, Double)) -> [Double]
mkLine (x, (s, b)) = replicate x 0 ++ [s, b] ++ replicate (floor r - x) 0

circleLines :: [[Double]]
circleLines = map mkLine circlePairs

circleImage :: [[(Int, Int, Int)]]
circleImage = map (map $ \x -> (floor $ 255 * x, 0, 0)) circleLines

mkLinePre :: [Int] -> [((Int, Int), Int)] -> [[(Int, Int)]]
mkLinePre [] _ = []
mkLinePre (y : ys) pixels =
	sort (map (first fst) (filter ((y ==) . snd . fst) pixels)) :
	mkLinePre ys pixels

circle8xps :: [[(Int, Int)]]
circle8xps = mkLinePre [0 .. floor $ sqrt (r ^ (2 :: Int) / 2)] circle8

-- mkLine2 :: [(Int, Int)] -> [Int]
-- mkLine2 

-- circleLines2 :: [[Int]]
-- circleLines2 
