module Controllers.Usuario where
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory
import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import Data.List (find)

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: Int,
    nome :: String,
    senha :: String
} deriving (Show, Generic)


data Atividade = Atividade {
    nomes :: String
} deriving (Show, Generic)

instance FromJSON Usuario
instance ToJSON Usuario

instance FromJSON Atividade
instance ToJSON Atividade


getUsuarioPorID :: Int-> [Usuario] -> Usuario
getUsuarioPorID _ [] = Usuario (-1) ""
getUsuairoPorID idUsuario (x:xs)
 | (idUsuario x) == idUsuario = x
 | otherwise = getUsuarioPorID idUsuario xs

removerPessoasPorID :: Int -> [Usuario] -> [Usuario]
removerPessoasPorID _ [] = []
removePessoasPorID idUsuario (x:xs)
 | (idUsuario x) == idUsuario = xs
 | otherwise = [x] ++ (removerPessoasPorID idUsuario xs)

getUsuario :: String -> [Usuario]
getUsuario path = do
 let file = unsafePerformIO( B.readFile path )
 let decodedFile = decode file :: Maybe [Usuario]
 case decodedFile of
  Nothing -> []
  Just out -> out

salvarUsuario :: String -> Int -> String -> String -> IO()
salvarUsuario jsonFilePath idUsuario nome senha = do
 let novoId = (length (getUsuario jsonFilePath)) + 1
 let ativ = Atividade "ok"
 let u = Usuario novoId nome senha [ativ]
 let userList = (getUsuario jsonFilePath) ++ [u]

 B.writeFile "../Temp.json" $ encode userList
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath



main :: IO()
main = do
    salvarUsuario "Database/usuarios.json" 1000 "Iris" "Iago"
    putStrLn (show (getUsuario "Databas/usuarios.json"))
