{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    ( gameServer
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
import Text.JSON.Generic
import Data.IORef

gameServer :: IORef Int -> IO ()
gameServer ref = do
  args <- map read <$> getArgs
  let port = if null args then 8889 else head args
  serverWith defaultConfig {srvLog = stdLogger, srvPort = port} $ handleRequest ref

handleRequest :: IORef Int -> Handler String
handleRequest ref addr url request
  | path == "move"  = handleMove direction
  | path == "shoot" = handleShot direction
  | path == "count" = countUp ref
  | otherwise       = pathError request
  where path      = url_path url
        direction = fst $ head $ url_params url

handleMove :: String -> IO (Response String)
handleMove d = return $ sendText OK ("Moving " ++ d ++ "\n")

handleShot :: String -> IO (Response String)
handleShot d = return $ sendText OK ("Shooting " ++ d ++ "\n")

pathError :: (Show a) => a -> IO (Response String)
pathError request = return $ sendText OK ("Bad Path\n" ++ (show request))

countUp ref = do
  modifyIORef ref (+1)
  val <- readIORef ref
  return $ sendText OK ((show val) ++ "\n")

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
                 } deriving (Show, Data)

data Position = Position { current :: Int
                         , previous :: Int
                         } deriving (Show, Data)

data GameMap = GameMap [Room] deriving (Show, Data)

gameMap =  GameMap [ Room 3  2  20 False False False
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
                    ]