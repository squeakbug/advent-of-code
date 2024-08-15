module Main where

import System.IO
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, first)
import Data.List (find)
import Data.Maybe (isJust)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

targetFunc :: [Int] -> Int
targetFunc [] = 0
targetFunc lst = 2 ^ (length lst - 1)

filterWinNumbers :: ([Int], [Int]) -> [Int]
filterWinNumbers (wnums, tnums) = filter pred wnums
    where
        pred wnum = isJust $ find (==wnum) tnums

getNumsFromLine :: String -> ([Int], [Int])
getNumsFromLine line = (map read (words wnums), map read (words tnums)) 
    where
        slpt = wordsWhen (=='|') . head . tail . wordsWhen (==':')
        (wnums : tnums : _) = slpt line

part1 :: [String] -> Int
part1 lns = sum $ map (targetFunc . filterWinNumbers . getNumsFromLine) lns

part2 :: [String] -> Int
part2 = sum . foldr acc [] . wins
  where
    acc = curry $ uncurry (:) . first ((+1) . sum . uncurry take) . (id &&& snd)
    wins = map (length . filterWinNumbers . getNumsFromLine)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFiles = lines content
    let part1_sln = part1 linesOfFiles
    let part2_sln = part2 linesOfFiles
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln
