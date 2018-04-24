module Main where

import Lib
import Data.IORef

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  gameServer ref