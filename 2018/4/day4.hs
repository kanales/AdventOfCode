
type Date = (Int,Int)
type Id = Int
type Asleep = [Bool]
type Record = (Date,Id,Asleep)

splitOn :: [Char] -> String -> [String]
splitOn _ [] = [""]
splitOn c (s:ss)
    | s `elem` c  = "" : rest
    | otherwise = (s: head rest) : tail rest
    where rest = splitOn c ss

