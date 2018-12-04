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

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = firstRepeated empty  . scanl (\acc x -> acc + x) 0 . cycle

main = do 
    stdin <- getContents
    putStr "Part 1: "
    let inpt = parseInput stdin
    print $ part1 inpt
    putStr "Part 2: "
    print $ part2 inpt