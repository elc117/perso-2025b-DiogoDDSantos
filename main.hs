{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
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

hostAny :: HostPreference
hostAny = "*"

initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS fullbottles (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ nome TEXT,\ 
  \ descricao TEXT)"

main :: IO ()
main = do
  conn <- open "fullbottles.db"
  initDB conn

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
    
    -- GET /healthz (check if the server is running)
    get "/healthz" $ text "ok"  

    -- GET /fullbottles
    get "/fullbottles" $ do
      fullbottles <- liftIO $ query_ conn "SELECT id, nome, descricao FROM fullbottles" :: ActionM [Fullbottles]
      json fullbottles

    -- GET /fullbottles/:id
    get "/fullbottles/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only idParam) :: ActionM [Fullbottles]
      if null result
        then status status404 >> json ("fullbottle not found" :: String)
        else json (head result)