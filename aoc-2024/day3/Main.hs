{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.IO
import Control.Applicative
import Data.Void (Void)
import Data.Maybe
import Data.Functor
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Text (pack, Text)
import Control.Monad.Combinators hiding (some)
import Text.Megaparsec hiding (some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

data Instruction = 
    Mul Int Int
  | Enable
  | Disable

type Parser = P.Parsec Void Text

parens :: Parser a -> Parser a
parens = P.between (string "(") (string ")")

parseMul :: Parser Instruction
parseMul = string "mul" *> parens (Mul <$> (L.decimal <* string ",") <*> L.decimal)

parser :: Parser [Instruction]
parser = catMaybes <$> some (go <* optional eol) <* eof
  where
    go =
      choice
        [ Just <$> try parseMul,
          Just Enable <$ try (string "do()"),
          Just Disable <$ try (string "don't()"),
          anySingle $> Nothing
        ]

exec :: Instruction -> Int
exec (Mul x y) = x * y
exec _ = 0

part1 :: String -> Int
part1 input = sum $ map exec result
    where
        Right result = runParser parser "" (pack input)

execSeq :: [Instruction] -> Bool -> Int -> Int
execSeq []             _     r = r
execSeq (Enable:xs)    _     r = execSeq xs True r
execSeq (Disable:xs)   _     r = execSeq xs False r
execSeq ((Mul x y):xs) True  r = execSeq xs True (r + x * y)
execSeq ((Mul x y):xs) False r = execSeq xs False r

part2 :: String -> Int
part2 input = execSeq result True 0
    where
        Right result = runParser parser "" (pack input)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    let part1_sln = part1 input
    let part2_sln = part2 input
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln