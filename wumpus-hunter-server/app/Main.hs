module Main where

import Lib
import Data.IORef

main :: IO ()
main = do
  gameState <- newIORef (newGame :: GameMap)
  gameServer gameState