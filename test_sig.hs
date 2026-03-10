{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main = do
  print $(stringE . show =<< [t| forall w m. [Int] -> Int |])
