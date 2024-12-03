module Main where

import System.IO
import Control.Monad.State
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort)
import qualified Data.Map as Map
import Control.Applicative (Alternative(empty))

type Report = [Int]

parseReport :: String -> Report
parseReport = map read
            . words

parseFile :: String -> [Report]
parseFile = map parseReport
          . lines

isReportValid :: Report -> Bool
isReportValid xs = isInRange folded && monotonic folded
    where
        monotonic xs = all (>0) xs || all (<0) xs
        isInRange xs = not $ any ((\x -> x == 0 || x > 3) . abs) xs
        folded = zipWith (-) xs (tail xs)

part1 :: [Report] -> Int
part1 = length . filter id . map isReportValid

part2 :: [Report] -> Int
part2 _ = 0

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    let input = parseFile content
    let part1_sln = part1 input
    let part2_sln = part2 input
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln