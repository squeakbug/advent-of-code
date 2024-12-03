module Main where

import System.IO
import Control.Monad.State
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort)
import qualified Data.Map as Map

-- TODO: Declare ZipM monad for operations on zipped sequences
-- TODO: It would be nice to see polymorphism over tuple size 
--    cause Zips with different tuple sizes are not the same types 

intoTuple2 :: [a] -> (a, a)
intoTuple2 [x, y] = (x, y)
intoTuple2 _      = error "List must have exactly 2 elements"

parseRow :: String -> (Int, Int)
parseRow = intoTuple2
         . map read
         . take 2
         . words

parseFile :: String -> [(Int, Int)]
parseFile = map parseRow 
          . lines

-- Dummy solution: I can use list of lists for mapping sort operaiont
-- but task declares only two lists
sortAndZip :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortAndZip xs = zip sortedFirst sortedSecond
  where
    (first, second) = unzip xs
    sortedFirst = sort first
    sortedSecond = sort second

part1 :: [(Int, Int)] -> Int
part1 = sum 
      . map (abs . uncurry (-)) 
      . sortAndZip

type MemoState k v a = State (Map.Map k v) a

-- | Memoize function
--   @input key - cache
--   @f - partially applied function, that gets cache and return result of computation
memoize :: (Ord k) => k -> (k -> v) -> MemoState k v v
memoize key f = do
    cache <- get
    case Map.lookup key cache of
        Just value -> return value
        Nothing -> do
            let newValue = f key
            put (Map.insert key newValue cache)
            return newValue

similarity :: [Int] -> Int -> Int
similarity [] s = 0
similarity (x:xs) s | x == s    = 1 + similarity xs s
                    | otherwise = similarity xs s

memoizedSimilarity :: Int -> MemoState Int Int Int
memoizedSimilarity xs = memoize xs similarity xs

-- Run sequence searches with the same cache
part2 :: [(Int, Int)] -> Int
part2 xs = sum $ map (\x -> x * similarity first x) second
    where
        (first, second) = unzip xs

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    let input = parseFile content
    let part1_sln = part1 input
    let part2_sln = part2 input
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln