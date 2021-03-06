import Prelude hiding (replicate, elem)
import System.Environment (getArgs)
import Data.Vector (Vector, accum, replicate, elem)
import qualified Data.Vector as Vector
import Data.Char (ord, chr)

hist :: Int -> [Int] -> Vector Int
hist n = accum (+) (replicate n 0) . fmap (\x -> (x,1))

digestVector :: Vector Int -> (Int, Int)
digestVector vec = (a,b)
                where   a = (if 2 `elem` vec then 1 else 0)
                        b = (if 3 `elem` vec then 1 else 0)
                     
combineTups :: (Num a) => [(a,a)] -> a
combineTups = uncurry (*) . foldl1 addTups
            where addTups (a,b) (a',b') = (a+a', b+b')

similarity :: String -> String -> (Int, String)
similarity xs ys = foldl foldingFunction (0,[]) (zip xs ys)
        where foldingFunction (ndif, lets) (a,b) = if a == b then (ndif,a:lets) else (ndif+1,lets)

-- fairly inefficient list concat
mkPairs :: [a] -> [(a, a)]
mkPairs [] = []
mkPairs (x:xs) = fmap ((,) x) xs ++ mkPairs xs

part1 :: String -> Int
part1 = combineTups . fmap (digestVector . hist 128 . fmap ord) . words

part2 :: String -> String
part2 = reverse . snd . head . filter ((==1) . fst) . fmap (uncurry similarity) . mkPairs . words

main = do 
    stdin <- getContents
    putStr "Part 1: "
    print $ part1 stdin
    putStr "Part 2: "
    print $ part2 stdin