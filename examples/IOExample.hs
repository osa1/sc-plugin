{-# LANGUAGE ScopedTypeVariables #-}

module Main where

main :: IO ()
main = do
    i :: Int <- readLn
    putStrLn $ "User entered " ++ show i
