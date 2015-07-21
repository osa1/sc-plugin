{-# LANGUAGE ScopedTypeVariables #-}

module Main where

main :: IO ()
main = do
  lst :: [Int] <- read <$> getLine
  putStrLn $ if null lst then "null" else show (head lst)
