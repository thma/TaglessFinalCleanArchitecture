{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module InterfaceAdapters.KVSSqliteTF where

import           Polysemy

import           Data.Aeson                     (decode, encode)
import           Data.Aeson.Types               (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8     (pack, unpack)
import           Data.Function                  ((&))
import           Data.Maybe                     (catMaybes, listToMaybe)
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Database.SQLite.Simple         (NamedParam ((:=)))
import qualified Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.Types
import           InterfaceAdapters.Config

import           UseCases.KVSasTF


data KeyValueRow = KeyValueRow T.Text T.Text
    deriving (Show)

instance FromRow KeyValueRow where
  fromRow = KeyValueRow <$> field <*> field

instance ToRow KeyValueRow where
  toRow (KeyValueRow key_ val) = toRow (key_, val)

-- | Run a KVStore effect against a SQLite backend. Requires a Config object as input.
instance (Show k, Read k, ToJSON v, FromJSON v) => Persistence k v IO where
  listAllKvs = listAction
  getKvs     = getAction
  insertKvs  = insertAction
  deleteKvs  = deleteAction 
  
instance Trace IO where
  trace _ = return () --putStrLn
  
instance Input IO Config where
  input = return Config {port = 8080, dbPath = "kvs-TF.db", backend = SQLite, verbose = False}
  

--getAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v) => k -> Sem r (Maybe v)
getAction key = do
  trace $ "getAction: " ++ show key
  conn <- connectionFrom input
  rows <- SQL.queryNamed conn
                      "SELECT key, value FROM store WHERE key = :key"
                      [":key" := show key] :: IO [KeyValueRow]
  case rows of
    []                          -> return Nothing
    (KeyValueRow _key value):xs -> return $ (decode . encodeUtf8) value


--listAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v) => Sem r [(k, v)]
listAction :: (Show k, Read k, ToJSON v, FromJSON v) => IO [(k, v)]
listAction = do
  trace "listAction:"
  conn <- connectionFrom input
  rows <- SQL.query_ conn "SELECT key, value FROM store" :: IO [KeyValueRow]
  let maybeList = map toKV rows
  return $ catNestedMaybe maybeList
    where
      toKV (KeyValueRow key value) =  ((read . T.unpack) key, (decode . encodeUtf8) value)
      catNestedMaybe [] = []
      catNestedMaybe ((key, Just value):xs) = (key, value):catNestedMaybe xs
      catNestedMaybe ((key, Nothing):xs)    = catNestedMaybe xs


--insertAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v) => k -> v -> Sem r ()
insertAction key value = do
  trace $ "insertAction: " ++ show key ++ " " ++ show (encode value)
  let (query, params) = ("INSERT INTO store (key, value) VALUES (:key, :value) "
                      <> "ON CONFLICT (key) DO UPDATE SET value = excluded.value",
                      [":key" := show key, ":value" := encodedValue])
                      where
                        encodedValue = (decodeUtf8 . encode) value
  conn <- connectionFrom input
  SQL.executeNamed conn query params


--deleteAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k) => k -> Sem r ()
deleteAction key = do
  trace $ "deleteAction: " ++ show key
  conn <- connectionFrom input
  SQL.executeNamed conn "DELETE FROM store WHERE key = :key" [":key" := show key]


-- | create a connection based on configuration data, make sure table "store" exists.
--connectionFrom :: (Member (Embed IO) r, Member Trace r) => Sem r Config -> Sem r SQL.Connection
connectionFrom c = do
  config <- c
  trace $ "open connection to: " ++ dbPath config
  getConnection (dbPath config)
    where
      getConnection :: FilePath -> IO SQL.Connection
      getConnection dbFile = do
        conn <- SQL.open dbFile
        SQL.execute_ conn "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
        return conn

--input :: IO Config
--input = return Config {port = 8080, dbPath = "kvs-TF.db", backend = SQLite, verbose = False}