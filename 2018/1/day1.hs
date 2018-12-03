import System.Environment (getArgs)
import Data.Set (Set, member, empty, insert)

parseInput :: String -> [Int]
parseInput = fmap readOp . words
        where readOp (op:num) = case op of  '+' -> read num
                                            '-' -> -(read num)

firstRepeated :: (Ord a) => Set a -> [a] -> a
firstRepeated s (x:xs)
                | x `member` s = x
                | otherwise  = firstRepeated (insert x s) xs    

part1 :: String -> String
part1 = show . sum . parseInput

part2 :: String -> String
part2 = show . firstRepeated empty  . scanl (\acc x -> acc + x) 0 . cycle . parseInput

main = do 
    arg <- head <$> getArgs
    interact (case arg of "1" -> part1
                          "2" -> part2)
    