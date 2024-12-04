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

data Instruction = Mul Int Int

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
          anySingle $> Nothing
        ]

exec :: Instruction -> Int
exec (Mul x y) = x * y

part1 :: String -> Int
part1 input = sum $ map exec result
    where
        Right result = runParser parser "" (pack input)

part2 :: String -> Int
part2 _ = 0

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    let part1_sln = part1 input
    let part2_sln = part2 input
    putStrLn $ printf "part1_sln = %d" part1_sln
    putStrLn $ printf "part2_sln = %d" part2_sln