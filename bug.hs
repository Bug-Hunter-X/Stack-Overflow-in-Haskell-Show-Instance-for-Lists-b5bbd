{-# LANGUAGE FlexibleInstances #-}

module Main where

class Show a where
  showsPrec :: Int -> a -> String

instance Show Int where
  showsPrec _ x = show x

instance Show a => Show [a] where
  showsPrec p xs = showList p xs
    where
      showList p []     = "[]"
      showList p (x:xs) = "[" ++ showsPrec p x ++ showList' p xs
      showList' p []    = "]"
      showList' p (x:xs) = "," ++ showsPrec p x ++ showList' p xs

main :: IO ()
main = print [1,2,3]