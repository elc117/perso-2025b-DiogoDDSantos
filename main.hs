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

--delcara o primeiro banco
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
    \ id_m INTEGER PRIMARY KEY,\
    \ nome_m TEXT,\ 
    \ descricao_m TEXT,\
    \ fullbottle1 INTEGER REFERENCES fullbottles(id),\
    \ fullbottle2 INTEGER REFERENCES fullbottles(id))"

-- Calcula se o id é impar 
isOdd :: Int -> Bool
isOdd x = x `mod` 2 == 1  

-- Se id da tabela for impar ou não, sera apontado o valor da melhor combinação
bestMatch :: Int -> Int
bestMatch x = if isOdd x then x + 1 else x - 1 


main :: IO ()
main = do

  conn <- open "fullbottles.db"
  initDB conn  -- Uma única chamada para inicializar ambas as tabelas

  mPort <- lookupEnv "PORT"
  let port = maybe 3000 Prelude.id (mPort >>= readMaybe)

  -- Mostra no terminal a porta que o servidor está rodando
  putStrLn $ "Server running on port:" ++ show port
  let opts = Options
        { verbose  = 1
        , settings = setHost hostAny $ setPort port defaultSettings
        }

  -- Área para funções do webhaskell(apelido carinhoso)
  scottyOpts opts $ do
    middleware logStdoutDev
    
    -- Interliga o webhaskell com o index.html
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file "index.html"

    -- GET responsavel por mostrar todas as fullbottles
    get "/fullbottles" $ do
      fullbottles <- liftIO $ query_ conn "SELECT id, nome, descricao FROM fullbottles" :: ActionM [Fullbottles]
      json fullbottles

    -- GET responsavel por mostrar a fullbottle e sua melhor combinação
    get "/fullbottles/:id" $ do
      idParam <- pathParam "id" :: ActionM Int

      -- seleciona a fullbottle com o id escolhido
      result  <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only idParam) :: ActionM [Fullbottles]

      -- seleciona a melhor combinação partindo do id escolhido
      result2 <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only (bestMatch idParam)) :: ActionM [Fullbottles]

      -- Tratamento de resultado
      let mainBottle = if null result then Nothing else Just (head result)
          bestBottle = if null result2 then Nothing else Just (head result2)
      if null result
        then status status404 >> json ("fullbottle not found" :: String)
        else json $ object ["main" .= mainBottle, "bestMatch" .= bestBottle]

    -- Verifica as combinações partindo dos ids escolhidos
    get "/bestmatch/:id1/:id2" $ do
      idParam1 <- pathParam "id1" :: ActionM Int
      idParam2 <- pathParam "id2" :: ActionM Int
      
      -- Seleciona a combinação, mesmo se estiver em ordem invertida
      result  <- liftIO $ query conn "SELECT id_m, nome_m, descricao_m, fullbottle1, fullbottle2 FROM bestmatch WHERE (fullbottle1 = ? AND fullbottle2 = ?) OR (fullbottle1 = ? AND fullbottle2 = ?)" (idParam1, idParam2, idParam2, idParam1) :: ActionM [Bestmatch]

    
      -- Busca informações das fullbottles
      fullbottle1 <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only idParam1) :: ActionM [Fullbottles]
      fullbottle2 <- liftIO $ query conn "SELECT id, nome, descricao FROM fullbottles WHERE id = ?" (Only idParam2) :: ActionM [Fullbottles]
      
      let fullbottle1Info = if null fullbottle1 then Nothing else Just (head fullbottle1)
          fullbottle2Info = if null fullbottle2 then Nothing else Just (head fullbottle2)
          combinationInfo = if null result then Nothing else Just (head result)
          isValidCombination = not (null result)
      
      json $ object
          [ "fullbottle1" .= fullbottle1Info
          , "fullbottle2" .= fullbottle2Info
          , "isValidCombination" .= isValidCombination
          , "bestmatch" .= combinationInfo
          ]
