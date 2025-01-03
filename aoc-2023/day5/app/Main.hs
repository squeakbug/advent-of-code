module Main where

import System.IO
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Functor.Identity (Identity)

type Rule  = (Int, Int, Int) -- (destination, start, range) 
type Table = [Rule]

-- Принимает строки первой группы
parseSeeds :: [String] -> [Int]
parseSeeds = map read . drop 1 . words . head

parseRule :: String -> Rule
parseRule str = (dst, start, range)
  where
    [dst, start, range] = map read $ words str

-- Принимает массив оставшихся групп
parseTables :: [String] -> [Table]
parseTables = map (map parseRule . drop 1 . splitOn "\n")

lookup' :: Table -> Int -> Int
lookup' [] x = x
lookup' ((dst, start, range) : ms) x
    | (x >= start) && (x < start + range) = dst + x - start
    | otherwise                   = lookup' ms x

locations :: [Table] -> [Int] -> [Int]
locations tables =  map (lookup' (tables !! 6))
                  . map (lookup' (tables !! 5))
                  . map (lookup' (tables !! 4))
                  . map (lookup' (tables !! 3))
                  . map (lookup' (tables !! 2))
                  . map (lookup' (tables !! 1))
                  . map (lookup' (tables !! 0))

part1 :: String -> Int
part1 content = minimum locs
    where 
        chunks = splitOn "\n\n" content
        lines = splitOn "\n" content
        seeds = parseSeeds $ splitOn "\n" $ head chunks
        tables = parseTables $ drop 1 chunks
        locs = locations tables seeds

transform :: Table -> ClosedInterval -> [ClosedInterval]
transform t (a,b)
    |rulesContain /= [] = [apply (head rulesContain) (a,b)]
    |rulesLeft    /= [] = transform t left1 ++ transform t right1
    |rulesRight   /= [] = transform t left2 ++ transform t right2
    |otherwise          = [(a,b)] 
  where
    -- Three possible cases:
    rulesContain = filter (((a,b) `isin`) . fst) $ t
    rulesLeft    = filter ((`cuts` (a,b)) . fst) $ t
    rulesRight   = filter (((a,b) `cuts`) . fst) $ t

    -- Case 0: Interval falls entirely into the domain of a rule.
    -- Nothing more to do.

    -- Case 1: A rule cuts the interval from the left
    left1        = (a, cut1)
    right1       = (cut1+1, b)
    cut1         = snd . fst . head $ rulesLeft

    -- Case 2: A rule cuts the interval from the right
    left2        = (a, cut2-1)
    right2       = (cut2, b)
    cut2         = fst . fst . head $ rulesRight

part2 :: String -> Int
part2 content = 0

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let part1_sln = part1 content
    let part2_sln = part2 content
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln