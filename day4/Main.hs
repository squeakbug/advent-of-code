module Main where

import System.IO
import System.Environment (getArgs)
import Text.Printf (printf)
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
        (wnums : tnums : _) = wordsWhen (=='|') $ head $ tail $ wordsWhen (==':') line

part1 :: [String] -> Int
part1 lns = sum $ map (targetFunc . filterWinNumbers . getNumsFromLine) lns

listAdding :: [Int] -> Int -> [Int]
listAdding _ 0 = []
listAdding [] cnt = 1 : listAdding [] (cnt - 1)
listAdding (rep : reps) cnt = (rep + 1) : listAdding reps (cnt - 1)

subFoldFunc:: [Int] -> [Int] -> Int -> Int
subFoldFunc (score : scores) (rep : reps) acc = subFoldFunc scores (listAdding reps score) (acc + score + score * rep)
subFoldFunc (score : scores) [] acc = subFoldFunc scores (listAdding [] score) (acc + score)
subFoldFunc [] _ acc = acc

-- foldFunc [1, 1] -> 1 + (1 + 1 * 1) = 3
-- foldFunc [1, 1, 1] -> 1 + (1 + 1 * 1) + (1 + 1 * 1) = 5
-- foldFunc [2, 2, 2] -> 2 + (2 + 1 * 2) + (2 + 2 * 2) = 12
-- foldFunc [4, 2, 2, 1, 0, 0] -> [4, 2+1, 2+1, 1+1, 0+1, 0] -> [1, ] -> 16 + 6 = 22

-- Задание
-- foldFunc [1, 2, 4, 8, 0, 0]
-- foldFunc [4, 2, 2, 1, 0, 0] -> [1, 2, 4, 8, 14, 1] = 30 
foldFunc :: [Int] -> Int
foldFunc scores = subFoldFunc scores [] 0

part2 :: [String] -> Int
part2 lns = foldFunc scores
    where 
        scores = map (targetFunc . filterWinNumbers . getNumsFromLine) lns

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFiles = lines content
    let part1_sln = part1 linesOfFiles
    let part2_sln = part2 linesOfFiles
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln
