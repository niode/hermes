module Utils
  ( mlist
  ) where

-- Extract values out of Maybes (ignore the Nothings).
mlist::[Maybe a]->[a]
mlist = foldr (\x xs -> case x of
  Nothing -> xs
  (Just a) -> a:xs) []
