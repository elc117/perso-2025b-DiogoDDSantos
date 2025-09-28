{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Fullbottles = Fullbottles
  { id   :: Int
  , nome     :: String
  , descricao    :: String
  } deriving (Show, Generic)

instance ToJSON Fullbottles
instance FromJSON Fullbottles

instance FromRow Fullbottles where
  fromRow = Fullbottles <$> field <*> field <*> field

instance ToRow Fullbottles where
  toRow (Fullbottles _ nome_ descricao_) = toRow (nome_, descricao_)

-- Declarando o segundo banco
data Bestmatch = Bestmatch
  { id_m  :: Int
  , nome_m  :: String
  , descricao_m :: String
  , fullbottle1 :: Int
  , fullbottle2 :: Int
  } deriving (Show, Generic)

instance ToJSON Bestmatch
instance FromJSON Bestmatch

instance FromRow Bestmatch where
  fromRow = Bestmatch <$> field <*> field <*> field <*> field <*> field

instance ToRow Bestmatch where
  toRow (Bestmatch _ nome_ descricao_ fullbottle1_ fullbottle2_) = toRow (nome_, descricao_, fullbottle1_, fullbottle2_)

hostAny :: HostPreference
hostAny = "*"

-- Inicialização única do banco de dados
initDB :: Connection -> IO ()
initDB conn = do
  -- Tabela fullbottles
  execute_ conn
    "CREATE TABLE IF NOT EXISTS fullbottles (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ nome TEXT,\ 
    \ descricao TEXT)"
  
  -- Tabela bestmatch
  execute_ conn
    "CREATE TABLE IF NOT EXISTS bestmatch (\
    \ id INTEGER PRIMARY KEY,\
    \ nome TEXT,\ 
    \ descricao TEXT,\
    \ fullbottle1 INTEGER REFERENCES fullbottles(id),\
    \ fullbottle2 INTEGER REFERENCES fullbottles(id))"

isOdd :: Int -> Bool
isOdd x = x `mod` 2 == 1  

bestMatch :: Int -> Int
bestMatch x = if isOdd x then x + 1 else x - 1 

main :: IO ()
main = do
  conn <- open "fullbottles.db"
  initDB conn  -- Uma única chamada para inicializar ambas as tabelas

  mPort <- lookupEnv "PORT"
  let port = maybe 3000 Prelude.id (mPort >>= readMaybe)

  putStrLn $ "Server running on port:" ++ show port
  let opts = Options
        { verbose  = 1
        , settings = setHost hostAny $ setPort port defaultSettings
        }

  scottyOpts opts $ do
    middleware logStdoutDev
    
    -- GET / (serve o frontend)
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file "index.html"

    -- GET /fullbottles
    get "/fullbottles" $ do
      fullbottles <- liftIO $ query_ conn "SELECT id, nome, descricao FROM fullbottles" :: ActionM [Fullbottles]
      json fullbottles

    -- GET /fullbottles/:id
    get "/fullbottles/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only idParam) :: ActionM [Fullbottles]
      result2 <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only (bestMatch idParam)) :: ActionM [Fullbottles]
      let mainBottle = if null result then Nothing else Just (head result)
          bestBottle = if null result2 then Nothing else Just (head result2)
      if null result
        then status status404 >> json ("fullbottle not found" :: String)
        else json $ object ["main" .= mainBottle, "bestMatch" .= bestBottle]