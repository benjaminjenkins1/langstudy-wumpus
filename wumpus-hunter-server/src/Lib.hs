{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    ( gameServer,
      GameState,
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
import Data.List

gameServer :: IORef GameState -> IO ()
gameServer gameState = do
  args <- map read <$> getArgs
  let port = if null args then 8889 else head args
  putStrLn $ "Listening on port " ++ (show port) ++ "..."
  serverWith defaultConfig {srvPort = port} $ handleRequest gameState

handleRequest :: IORef GameState -> Handler String
handleRequest gameState addr url request
  | path == "moveplayer"  = handleMovePlayer gameState direction
  | path == "shoot" = handleShot gameState direction
  | path == "state" = showState gameState
  | otherwise       = pathError request
  where path      = url_path url
        direction = fst $ head $ url_params url

handleMovePlayer :: IORef GameState -> String -> IO (Response String)
handleMovePlayer gameState d = do
  putStrLn "Player moved"
  modifyIORef gameState (movePlayer d)
  newState <- readIORef gameState
  message <- return $ postMoveMessage newState
  return $ sendText OK (postMoveMessage newState)

handleShot :: IORef GameState -> String -> IO (Response String)
handleShot gameState d = do
  putStrLn "Player fired an arrow"
  return $ sendText OK ("Shooting\n")

pathError :: (Show a) => a -> IO (Response String)
pathError request = return $ sendText OK ("Bad Path\n" ++ (show request))

-- For debugging
showState :: IORef GameState -> IO (Response String)
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
                 , pit :: Bool
                 , bats :: Bool
                 } deriving (Show)

data Position = Position { current :: Int
                         , previous :: Int
                         } deriving (Show)

data GameState = GameState { rooms :: [Room]
                           , pos :: Position
                           , encounteredBats :: Bool
                           , gameIsOver :: Bool
                           } deriving (Show)

newGame =  GameState  {rooms = [ Room 1  3  2  False False False
                               , Room 0  4  18 True  False False
                               , Room 5  0  6  False False False
                               , Room 19 7  0  False False False
                               , Room 10 1 5   False False False
                               , Room 8  4  2  False True  False
                               , Room 7  9  2  False False False
                               , Room 6  3  14 False False True
                               , Room 5  9  11 False False False
                               , Room 6  13 8  False True  False
                               , Room 4  11 15 False False False
                               , Room 10 8  12 False False True
                               , Room 11 13 16 False False False
                               , Room 12 9  14 False False False
                               , Room 13 7  17 False False False
                               , Room 10 16 18 False False False
                               , Room 12 17 15 False True  False
                               , Room 16 4  19 False False False
                               , Room 1  15 19 False False False
                               , Room 18 17 3  False False False
                               ]
                      , pos = (Position 0 3)
                      , encounteredBats = False
                      , gameIsOver = False}

-- Move in a direction
movePlayer :: String -> GameState -> GameState
movePlayer d gameState = 
  let currentPos       = current $ pos gameState
      previousPos      = previous $ pos gameState
      roomsList        = rooms gameState
      wumpusPos        = getWumpusPosition gameState -- Just Int
      newPos           = nextRoom roomsList currentPos previousPos d
      batsPositions    = getBatsPositions gameState
      pitPositions     = getPitPositions gameState
      didEncounterBats = if elemIndex newPos batsPositions /= Nothing then True else False 
      gameOver         = if gameIsOver gameState == True 
                         then True
                         else if or [elemIndex currentPos pitPositions /= Nothing, Just currentPos == wumpusPos]
                              then True
                              else False
  in GameState {rooms=(rooms gameState), pos=(Position newPos currentPos), encounteredBats = didEncounterBats, gameIsOver = gameOver}

-- Shoot in a direction


-- Generate post-move message
postMoveMessage :: GameState -> String
postMoveMessage gameState = 
  let currentPos        = current $ pos gameState
      previousPos       = previous $ pos gameState
      wumpusPos         = getWumpusPosition gameState -- Just Int
      pitPositions      = getPitPositions gameState
      adjacencyMessages = getAdjacencyMessages
  in if gameIsOver gameState == True
     then "The game is over."
     else if encounteredBats gameState == True
          then "You were carried to another room by bats.\n"
          else if elemIndex currentPos pitPositions /= Nothing
                then "You fell to your death into a pit. RIP.\n"
                else if Just currentPos == wumpusPos
                    then "You were mauled by a wumpus. RIP.\n"
                    else adjacencyMessages gameState currentPos

-- Utility functions

nextRoom :: [Room] -> Int -> Int -> String -> Int
nextRoom rooms current previous direction
  | direction == "L" = nextLeft currentRoom previous
  | direction == "R" = nextRight currentRoom previous
  | direction == "B" = previous
  where currentRoom = rooms !! current

nextLeft :: Room -> Int -> Int
nextLeft room previous
  | previous == (adjA room) = adjB room
  | previous == (adjB room) = adjC room
  | previous == (adjC room) = adjA room

nextRight :: Room -> Int -> Int
nextRight room previous
  | previous == (adjA room) = adjC room
  | previous == (adjB room) = adjA room
  | previous == (adjC room) = adjB room

getWumpusPosition :: GameState -> Maybe Int
getWumpusPosition gameState = 
  let roomsList  = rooms gameState
      wumpusList = map (\x -> wumpus x) roomsList
  in elemIndex True wumpusList

getPitPositions :: GameState -> [Int]
getPitPositions gameState = 
  let roomsList = rooms gameState
      pitsList  = map (\x -> pit x) roomsList
  in elemIndices True pitsList

getBatsPositions :: GameState -> [Int]
getBatsPositions gameState =
  let roomsList = rooms gameState
      batsList  = map (\x -> bats x) roomsList
  in elemIndices True batsList

getAdjacencyMessages :: GameState -> Int -> String
getAdjacencyMessages gameState pos = "Adjacency messages\n"