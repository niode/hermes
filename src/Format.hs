module Format
  ( list
  , numberList
  , appendDescription
  , adjectives
  , nouns
  , item
  ) where

import Data.List (intersperse)

-- Turn a list of strings into one string with newlines.
list::[String]->String
list = concat . (intersperse "\n")

-- Add numbers to the start of each item.
numberList::[String]->String
numberList = concat . (intersperse "\n")
  . (map (\(n, s) -> "[" ++ (show n) ++ "]: " ++ s))
  . (zip [1..])

-- Append text to an existing game description.
appendDescription::String->String->String
appendDescription old new = old ++ "\n\n" ++ new

-- Format an item description. (adjectives, nouns).
item::[String]->[String]->String
item a n = (adjectives a) ++ " " ++ (nouns n)

-- Format a list of adjectives.
adjectives::[String] -> String
adjectives = foldr (\word words -> case words of
  [] -> word                    -- Nothing, just put the last word.
  _  -> word ++ ", " ++ words)  -- Put this word before the other words.
  []

-- Format a list of nouns.
nouns::[String] -> String
nouns = foldr (\word words -> case words of
  [] -> word                    -- Nothing, just put the last word.
  _  -> word ++ " " ++ words)   -- Put this word before the other words.
  []
