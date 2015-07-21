module Main where

-- optimizes fine (with -O)
sum_foldr :: [Int] -> Int
sum_foldr       = foldr (+) 0

-- optimizes fine (with -O)
and_foldr :: [Bool] -> Bool
and_foldr       = foldr (&&) True

-- decimal_foldr :: Int -> Int
-- decimal_foldr   = foldr (\d x -> (fromInteger d + x) / 10) 0

-- this is pretty awesome, GHC optimizes this to id with -O. I think there are
-- multiple passes involved in this, but I'm not sure exactly which passes.
id_foldr :: [a] -> [a]
id_foldr        = foldr (:) []

-- optimizes fine with -O
length_foldr :: [a] -> Int
length_foldr    = foldr (\x n -> 1 + n) 0

-- optimizes fine with -O
map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f     = foldr ((:) . f) []

-- optimizes fine with -O
filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr p  = foldr (\x xs -> if p x then x : xs else xs) []

-- optimizes fine with -O
concat_foldr :: [[a]] -> [a]
concat_foldr    = foldr (++) []

-- optimizes fine, but quadratic. TODO: Is it possible to improve this somehow?
reverse_foldr :: [a] -> [a]
reverse_foldr   = foldr snoc [] where snoc x xs = xs ++ [x]

-- THIS IS RIDICULOUS!! this is compiled to (++). How is this even possible?
-- (with -O)
app_foldr :: [a] -> [a] -> [a]
app_foldr xs ys = foldr (:) ys xs

inits :: [a] -> [[a]]
inits           = foldr (\x xss -> [] : map (x:) xss) [[]]

tails :: [a] -> [[a]]
tails           = foldr (\x xss -> (x : head xss) : xss) [[]]

main :: IO ()
main = do
    let lsti = [1, 2, 3]
        lstb = [True, True, False]
    print $ sum_foldr lsti
    print $ and_foldr lstb
    print $ id_foldr  lsti
    print $ length_foldr lsti
    print $ map_foldr (+1) lsti
    print $ filter_foldr even lsti
    print $ concat_foldr [lsti, lsti]
    print $ reverse_foldr lsti
    print $ app_foldr lsti lsti
    print $ inits lsti
    print $ tails lsti
