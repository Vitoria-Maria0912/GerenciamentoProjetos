{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.CaixadeMensagem where
import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import System.Directory ()
import System.IO ()
import Data.Aeson
import Data.List 
import Data.Typeable
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.IO.Unsafe
import Controllers.Usuario
import System.Directory


instance FromJSON CaixadeMensagem
instance ToJSON CaixadeMensagem

data CaixadeMensagem =  CaixadeMensagem{
   idCaixadeMensagem :: Int,
   nomeRemetente :: String,
   conteudo :: String
 }deriving (Show, Generic)

-- | Função que salva e escreve o usuario no arquivo diretamente
salvarCaixadeMensagem :: String -> Int -> String ->  String -> IO()
salvarCaixadeMensagem jsonFilePath idUs nome conteudo  = do
  let usuario = CaixadeMensagem idUs nome conteudo  
  let userList = (getMensagens jsonFilePath) ++ [usuario]

  B.writeFile "../Temp.json" $ encode userList
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

-- função que pega todas mensagens do sistema
getMensagens :: String -> [CaixadeMensagem]
getMensagens file = do
  let fil = unsafePerformIO(B.readFile file)
  let decodedFile = decode fil :: Maybe [CaixadeMensagem]
  case decodedFile of
    Nothing -> []
    Just mensagens -> mensagens

--formata mensagens de envio dos membros
formataMensagem :: String -> String -> String
formataMensagem nomeusuario mensagem = "Enviada por " ++ nomeusuario ++ " ✔ \n'" ++ mensagem ++ "'\n"


-- Função que exibe mensagens a partir de uma caixa de mensagens específica
exibeMensagens :: [CaixadeMensagem] -> Int -> IO ()
exibeMensagens caixas idDesejado = do
    let caixasEncontradas = filter (\caixa -> idCaixadeMensagem caixa == idDesejado) caixas
    if null caixasEncontradas
        then putStrLn $ "Caixa de Mensagens  - ID " ++ show idDesejado ++ " vazia\n"
        else do
            putStrLn $ "Caixa de Mensagens - ID " ++ show idDesejado ++ " :"
            
            mapM_ (\caixa -> putStrLn $  "\n" ++ formataMensagem (nomeRemetente caixa) (conteudo caixa)) caixasEncontradas

getType :: Typeable a => a -> String
getType x = show (typeOf x)
