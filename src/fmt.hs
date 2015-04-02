module Utils
  ( mlist
  , numberList
  ) where

-- Extract values out of Maybes (ignore the Nothings).
mlist::[Maybe a]->[a]
mlist = foldr (\x xs -> case x of
  Nothing -> xs
  (Just a) -> a:xs) []

numberList::[String]->[String]
numberList = (map (\(n, s) -> "[" ++ (show n) ++ "]: " ++ s)) . (zip [1..])
