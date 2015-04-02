module Format
  ( list
  , numberList
  , appendDescription
  ) where

import Data.List (intersperse)

list::[String]->String
list = concat . (intersperse "\n")

numberList::[String]->String
numberList = concat . (intersperse "\n")
  . (map (\(n, s) -> "[" ++ (show n) ++ "]: " ++ s))
  . (zip [1..])

appendDescription::String->String->String
appendDescription old new = old ++ "\n\n" ++ new
