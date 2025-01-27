{-# LANGUAGE FlexibleInstances #-}

module Main where

class Show a where
  showsPrec :: Int -> a -> String

instance Show Int where
  showsPrec _ x = show x

instance Show a => Show [a] where
  showsPrec p xs = showList p xs
    where
      showList p = go
        where
          go []     = "[]"
          go (x:xs) = "[" ++ showsPrec p x ++ rest
            where
              rest = case xs of
                        [] -> "]"
                        _ -> "," ++ go xs

main :: IO ()
main = print [1,2,3]