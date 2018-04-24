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
  | path == "state" = showState gameState 
  | otherwise       = pathError request
  where path      = url_path url
        direction = fst $ head $ url_params url

handleMove :: IORef GameMap -> String -> IO (Response String)
handleMove gameState d = return $ sendText OK ("Moving\n")

handleShot :: IORef GameMap -> String -> IO (Response String)
handleShot gameState d = return $ sendText OK ("Shooting\n")

pathError :: (Show a) => a -> IO (Response String)
pathError request = return $ sendText OK ("Bad Path\n" ++ (show request))

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

newGame =  GameMap ([ Room 3  2  20 False False False
                    , Room 4  6  1  False False False
                    , Room 7  5  1  False False False
                    , Room 5  10 2  False False False
                    , Room 3  11 4  False False False
                    , Room 2  8  19 False False False
                    , Room 18 9  3  False False False
                    , Room 10 12 6  False False False
                    , Room 7  15 11 False False False
                    , Room 13 8  4  False False False
                    , Room 9  13 15 False False False
                    , Room 14 16 8  False False False
                    , Room 14 10 11 False False False
                    , Room 13 15 12 False False False
                    , Room 9  17 14 False False False
                    , Room 12 17 19 False False False
                    , Room 15 18 6  False False False
                    , Room 7  20 17 False False False
                    , Room 20 6  16 False False False
                    , Room 18 1  19 False False False
                    ], Position 1 20)