{-# LANGUAGE MagicHash #-}

module Main where

import PartialEvaluation.Annotations

import GHC.Prim
import GHC.Types

{-# ANN myAdd PluginInline #-}
myAdd :: Int -> Int -> Int
myAdd (I# i1) (I# i2) = I# (i1 +# i2)

main :: IO ()
main = do
    print $ myAdd 1 2
    print (I# (1# +# 2#))
