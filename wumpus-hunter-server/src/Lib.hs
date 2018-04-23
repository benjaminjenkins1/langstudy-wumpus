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

gameServer :: IO ()
gameServer = do
    args <- map read <$> getArgs
    let port = if null args then 8889 else head args
    serverWith defaultConfig {srvLog = stdLogger, srvPort = port} $ handleRequest

handleRequest :: Handler String
handleRequest addr url request
    | path == "move"  = handleMove direction
    | path == "shoot" = handleShot direction
    | otherwise       = pathError
    where path      = url_path url
          direction = fst $ head $ url_params url

handleMove :: String -> IO (Response String)
handleMove d = return $ sendText OK ("Moving " ++ d ++ "\n")

handleShot :: String -> IO (Response String)
handleShot d = return $ sendText OK ("Shooting " ++ d ++ "\n")

pathError :: IO (Response String)
pathError = return $ sendText OK "Bad Path\n"

sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/plain"
             $ (respond s :: Response String) {rspBody = txt}
    where txt = encodeString v
