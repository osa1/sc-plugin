module Main where

data T = A | B

a :: T
a = A

b :: T
b = B

knownCase :: Bool
knownCase = case a of { A -> True; B -> False }

main :: IO ()
main = do
    print knownCase
    return ()
