module Main where

import System.IO
import System.Environment (getArgs)
import Text.Printf (printf)
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, first)
import Data.List (find)
import Data.Maybe (isJust)

parseFile :: [String] -> ([String], [String])
parseFile = ((!! 0) &&& (!! 1))
      . fmap (tail . words)

-- (t_1 + t_2 = t) && (t_1 * t_2 >= d)
-- ((t_1)^2 + t * t_1 + d = 0) => t_1 = (t +/- sqrt (4 * d)) / 2
solve :: (Int, Int) -> Int
solve (time, distance) | even time = delta2       `div` 2 * 2 + 1
                       | otherwise = (delta2 + 1) `div` 2 * 2
  where
    delta2  = floor $ sqrt (fromIntegral $ time ^ 2 - 4 * distance)

part1 :: ([String], [String]) -> Int
part1 = product
      . fmap (solve . bimap read read)
      . uncurry zip

part2 :: ([String], [String]) -> Int
part2 = solve
      . bimap num num
  where num = read . concat

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let linesOfFiles = (parseFile . lines) content
    let part1_sln = part1 linesOfFiles
    let part2_sln = part2 linesOfFiles
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln