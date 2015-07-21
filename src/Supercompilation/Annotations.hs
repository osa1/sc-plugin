{-# LANGUAGE DeriveDataTypeable #-}

module Supercompilation.Annotations where

import Data.Data

--------------------------------------------------------------------------------

type ConditionStr = String

data PEAnnotation
  = Terminates ConditionStr
  deriving (Data, Typeable)
