{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.IP (fromSockAddr)
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import qualified Data.Text as T
import Network.Wai (remoteHost)
import Web.Scotty
import System.Environment (getEnv)

data CSPReport =
  CSPReport { idNum :: Maybe Integer
            , report :: A.Object
            , allHeaders :: String
            , ip :: Maybe String
            } deriving (Eq, Show)

instance ToRow CSPReport where
  toRow r = [ toJSONField (report r)
            , toField (allHeaders r)
            , toField (ip r)
            ]

insertDB :: Connection -> CSPReport -> IO Int64
insertDB conn csp =
  execute conn "insert into csp_report(body, headers, ip) values (?, ?, ?)" csp
            
main :: IO ()
main = do
  dbConn <- BS.pack <$> getEnv "DB_CONN"
  conn <- connectPostgreSQL dbConn
  execute_ conn $
    "create table if not exists csp_report (\
    \  id serial primary key,\
    \  body text not null,\
    \  headers text not null,\
    \  ip varchar(39));"
  scotty 8080 $ do
    post "/report/csp" $ do
      report <- body
      allHeaders <- headers
      req <- request
      let ipAddr = show . fst <$> fromSockAddr (remoteHost req)
      case A.decode report :: Maybe A.Object of
        Just r -> do
          liftIO $ insertDB conn (CSPReport Nothing r (show allHeaders) ipAddr)
          text ":)"
        Nothing -> text ":("
    get "/healthcheck" $ do
      [Only r] <- liftIO $ query_ conn "select 'working';"
      text r
