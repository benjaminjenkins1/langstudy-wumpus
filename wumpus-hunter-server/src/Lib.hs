{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    ( gameServer,
      GameMap,
      newGame
    ) where

import Codec.Binary.UTF8.String
import Network.BufferType
import Network.HTTP.Server (defaultConfig,
                            insertHeader,
                            respond,
                            rqBody,
                            rspBody,
                            serverWith,
                            srvLog,
                            srvPort,
                            Handler,
                            HeaderName(HdrContentLength,HdrContentEncoding),
                            Response,
                            StatusCode(OK)
                            )
import Network.HTTP.Server.Logger (stdLogger)
import Network.URL
import System.Random (getStdGen,randomR)
import System.Environment (getArgs)
import Data.IORef

gameServer :: IORef GameMap -> IO ()
gameServer gameState = do
  args <- map read <$> getArgs
  let port = if null args then 8889 else head args
  putStrLn $ "Listening on port " ++ (show port) ++ "..."
  serverWith defaultConfig {srvPort = port} $ handleRequest gameState

handleRequest :: IORef GameMap -> Handler String
handleRequest gameState addr url request
  | path == "move"  = handleMove gameState direction
  | path == "shoot" = handleShot gameState direction
  | path == "state" = showState gameState -- For debugging
  | otherwise       = pathError request
  where path      = url_path url
        direction = fst $ head $ url_params url

handleMove :: IORef GameMap -> String -> IO (Response String)
handleMove gameState d = return $ sendText OK ("Moving\n")

handleShot :: IORef GameMap -> String -> IO (Response String)
handleShot gameState d = return $ sendText OK ("Shooting\n")

pathError :: (Show a) => a -> IO (Response String)
pathError request = return $ sendText OK ("Bad Path\n" ++ (show request))

-- For debugging
showState :: IORef GameMap -> IO (Response String)
showState gameState = do
  currentState <- readIORef gameState
  return $ sendText OK (show currentState) 

sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/plain"
             $ (respond s :: Response String) {rspBody = txt}
  where txt = encodeString v

data Room = Room { adjA :: Int
                 , adjB :: Int
                 , adjC :: Int
                 , wumpus :: Bool
                 , bats :: Bool
                 , pit :: Bool
                 } deriving (Show)

data Position = Position { current :: Int
                         , previous :: Int
                         } deriving (Show)

data GameMap = GameMap ([Room], Position) deriving (Show)

newGame =  GameMap ([ Room 2  1  19 False False False
                    , Room 3  5  0  False False False
                    , Room 6  4  0  False False False
                    , Room 4  9  1  False False False
                    , Room 2  10 3  False False False
                    , Room 1  7  18 False False False
                    , Room 17 8  2  False False False
                    , Room 9  11 5  False False False
                    , Room 6  14 10 False False False
                    , Room 12 7  3  False False False
                    , Room 8  12 14 False False False
                    , Room 13 15 7  False False False
                    , Room 13 9  10 False False False
                    , Room 12 14 11 False False False
                    , Room 8  16 15 False False False
                    , Room 11 16 18 False False False
                    , Room 14 17 5  False False False
                    , Room 6  19 16 False False False
                    , Room 19 5  15 False False False
                    , Room 17 0  18 False False False
                    ], Position 0 19)