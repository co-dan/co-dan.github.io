{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Prim

fib :: Int -> Int
fib n = fibs !! n
  where fibs = 0:1:zipWith (+) fibs (tail fibs)

fibAction :: JSRef () -> Int -> IO ()
fibAction ref n = do
    res <- toJSRef (fib n)
    setProp ("result"::Text) res ref
        
main :: IO ()
main = return ()
