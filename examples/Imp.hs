-- IMP language from Software
-- Foundations(http://www.cis.upenn.edu/~bcpierce/sf/current/Imp.html).
--
-- AST should be eliminated from the compiled program.
--
module Main where

data AExp
  = ANum Int
  | APlus AExp AExp
  | AMinus AExp AExp
  | AMult AExp AExp

data BExp
  = BTrue
  | BFalse
  | BEq AExp AExp
  | BLe AExp AExp
  | BNot BExp
  | BAnd BExp BExp

aeval :: AExp -> Int
aeval (ANum i)          = i
aeval (APlus e1 e2)     = aeval e1 + aeval e2
aeval (AMinus e1 e2)    = aeval e1 - aeval e2
aeval (AMult e1 e2)     = aeval e1 * aeval e2

beval :: BExp -> Bool
beval BTrue             = True
beval BFalse            = False
beval (BEq e1 e2)       = aeval e1 == aeval e2
beval (BLe e1 e2)       = aeval e1 < aeval e2
beval (BNot e)          = not $ beval e
beval (BAnd e1 e2)      = beval e1 && beval e2

example1 :: BExp
example1 = BLe (ANum 2) (ANum 3) -- doesn't terminate
-- example1 = BLe (ANum 2) (APlus (ANum 1) (ANum 2)) -- generates offending program

example1_ret :: Bool
example1_ret = beval example1

-- example1_ret :: Bool
-- example1_ret = beval $ BEq (APlus (ANum 1) (ANum 2)) (ANum 3)

-- example1_ret :: Int -> Bool
-- example1_ret i = beval $ BEq (ANum 10) (ANum i)

main :: IO ()
main = do
    print example1_ret
    -- ln <- readLn
    -- print $ example1_ret (read ln)
