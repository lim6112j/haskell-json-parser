module Main (main) where

import Lib

data JsonValue = JsonNull | JsonInt Integer | JsonString String deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

main :: IO ()
main = someFunc
