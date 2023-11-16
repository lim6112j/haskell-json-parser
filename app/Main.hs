module Main (main) where

import Lib

data JsonValue = JsonNull | JsonInt Integer | JsonString String deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser a) = Parser $ \input -> do
    (input', x) <- a input
    return (input', f x)

instance Applicative Parser where
  pure a = Parser $ \input -> do
    return (input, a)
  (Parser f) <*> (Parser a) = Parser $ \input -> do
    (input', g) <- f input
    (input'', x) <- a input'
    return (input'', g x)

instance Monad Parser where
  (Parser a) >>= f = Parser $ \input -> do
    (input', x) <- a input
    runParser (f x) input'

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP str = undefined

main :: IO ()
main = do
  let f = runParser $ sequence $ charP <$> "hel"
  print $ f "hello"
