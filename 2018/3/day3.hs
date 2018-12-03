import System.Environment (getArgs)
import Data.Array.Unboxed
import Debug.Trace (traceShow)

type Rect = (Int,(Int,Int),(Int,Int))
type IntArray = UArray (Int,Int) Int

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (s:ss)
    | s == c = "" : rest
    | otherwise = (s: head rest) : tail rest
    where rest = splitOn c ss

toTup2 :: [a] -> (a,a)
toTup2 (x:y:[]) = (x,y)

maxtups :: [(Int,Int)] -> (Int, Int)
maxtups = (\(xs,ys) -> (maximum xs, maximum ys)) . unzip

parseRow :: String -> Rect
parseRow = parseList . words
      where parseList (i:_:p:s:_) = let id_ = read $ drop 1 i
                                        pos = fmap read $ splitOn ',' $ init p
                                        siz = fmap read $ splitOn 'x' s
                                    in (id_,toTup2 pos, toTup2 siz)

addTups :: (Num a) => (a,a) -> (a,a) -> (a,a)
addTups (a,b) (a',b') = (a+a', b+b')

getRects :: String -> [Rect]
getRects = fmap parseRow . lines

overlappingMatrix :: [Rect] -> IntArray
overlappingMatrix rects = accumArray accFun 0 ((0,0),(n,m)) $ asoc rects
                    where (is,poss,sizs) = unzip3 rects
                          (n,m) = (maxtups poss) `addTups` (maxtups sizs)
                          accFun x i 
                            | x == 0 = i
                            | otherwise = -1

asoc :: [Rect] -> [((Int,Int),Int)]
asoc = concatMap (\(i,(x,y),(h,w)) -> [((k,j),i) | k <- [x..x+h-1], j <- [y..y+w-1]])

freeSpace :: [Rect] -> IntArray -> Int
freeSpace (r:rs) ar 
    | all (\x -> ar!x == i) indices = i
    | otherwise = freeSpace rs ar
    where (i,(x,y),(h,w)) = r
          indices = [(k,j) | k <- [x..x+h-1], j <- [y..y+w-1]]

part1 :: String -> String
part1 = show . foldl1 (\acc x-> if x == -1 then acc + 1 else acc) . elems . overlappingMatrix . getRects

part2 :: String -> String
part2 s = show $ freeSpace rs $ overlappingMatrix rs
    where rs = getRects s

main = do 
    arg <- head <$> getArgs
    interact (case arg of "1" -> part1
                          "2" -> part2)