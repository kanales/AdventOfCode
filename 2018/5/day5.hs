import System.Environment (getArgs)
import Data.Char (toUpper)
import Data.List (nub)
import Control.Applicative ((<*>))

sameType :: Char -> Char -> Bool
sameType a b = (toUpper a) == (toUpper b)

(=*=) :: Char -> Char -> Bool
a =*= b = (a `sameType` b) && (a /= b)

digestString :: String -> String
digestString = foldl foldF []
        where foldF [] a = [a]
              foldF (x:xs) a 
                    | x =*= a   = xs
                    | otherwise = (a:x:xs)




part1 :: String -> Int 
part1 = length . digestString

filterAndDigest :: String -> String -> [Int]
filterAndDigest chars s = (fmap filterF chars) <*> pure s
     where filterF c = part1 . filter ( (/=c) . toUpper)

part2 :: String -> Int
part2 s = minimum $ filterAndDigest (nub $ fmap toUpper s) s


main = do
    stdin <- getContents
    print $ part1 stdin
    print $ part2 stdin