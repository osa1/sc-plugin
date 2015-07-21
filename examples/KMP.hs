module Main where

data Ch = A | B

alphabetEq :: Ch -> Ch -> Bool
alphabetEq x y = case x of
    A -> case y of A -> True
                   _ -> False
    B -> case y of B -> True
                   _ -> False

match :: [Ch] -> [Ch] -> Bool
match p s = loop p s p s

loop :: [Ch] -> [Ch] -> [Ch] -> [Ch] -> Bool
loop pp0 ss0 op os =
  case pp0 of
    []     -> True
    (p:pp) ->
      case ss0 of
        []     -> False
        (s:ss) -> if alphabetEq p s then loop pp ss op os else next op os

next :: [Ch] -> [Ch] -> Bool
next op ss0 =
  case ss0 of
    []     -> False
    (_:ss) -> loop op ss op ss

root :: [Ch] -> Bool
root u = match [A, A, B] u

main :: IO ()
main = do
  print $ root [] == False
  print $ root [A] == False
  print $ root [A, B] == False
  print $ root [A, A, B] == True
  print $ root [A, A, B, A] == True
  print $ root [A, B, A] == False
