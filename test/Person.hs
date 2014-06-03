{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Person where

import           GHC.Generics
import           Data.Aeson

data Person = Person {
  name :: String
, age :: Int
} deriving (Eq, Show, Generic)

instance ToJSON Person
