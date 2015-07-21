module Main where

import PartialEvaluation.Annotations

-- match :: [Int] -> Int
-- match [x, y, z] = x + y + z
-- match []        = 0
-- match _         = 1
--
-- match2 :: Maybe (Maybe a) -> a
-- match2 (Just (Just a)) = a

-- -# ANN add1 (Terminates "") #-}
-- -# NOINLINE add1 #-}
-- add1 :: Int -> Int -> Int -> Int
-- add1 i1 i2 i3 = i1 + i2 + i3

-- add1_1 :: Int -> Int -> Int -> Int
-- add1_1 i1 i2 = \i3 -> i1 + i2 + i3
--
-- add1_2 :: Int -> Int -> Int -> Int
-- add1_2 i1 = \i2 -> \i3 -> i1 + i2 + i3
--
{-# ANN fac (Terminates "$1 >= 0") #-}
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

main :: IO ()
main = do
    -- print $ match [1, 2, 3]
    -- print $ match2 (return $ return 10)
    -- print $ add1 1 2 3
    print $ fac 5
    -- print $ 1 + 2 + 3
