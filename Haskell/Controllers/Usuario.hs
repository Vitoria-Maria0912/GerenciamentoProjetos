module Controllers.Usuario where
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory

-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: Int,
    nome :: String,
    senha :: String
} deriving (Show, Generic)


instance FromJSON Usuario
instance ToJSON Usuario

getUsarioPorID :: Int-> [Usuario] -> Usuario
getUsarioPorID _ [] = Usuario (-1) "" ""
getUsarioPorID idUsuarioS (x:xs)     
  | (idUsuario x) == idUsuarioS = x
  | otherwise = getUsarioPorID idUsuarioS xs


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
 let u = Usuario novoId nome senha
 let userList = (getUsuario jsonFilePath) ++ [u]

 B.writeFile "../Temp.json" $ encode userList
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath

 -- remove usuario pelo ID
removeUsarioPorID :: Int -> [Usuario] -> [Usuario]
removeUsarioPorID _ [] = []
removeUsarioPorID idUsuarioS(x:xs)
 | (idUsuario x) == idUsuarioS = xs
 | otherwise = [x] ++ (removeUsarioPorID idUsuarioS xs)

removerUsuario :: String -> Int -> IO()
removerUsuario jsonFilePath idUsuario = do
 let usuarios = getUsuario jsonFilePath
 let novosUsuarios = removeUsarioPorID idUsuario usuarios

 B.writeFile "../Temp.json" $ encode novosUsuarios
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath

getNumDeUsuarios :: String -> Int
getNumDeUsuarios jsonFilePath = length (getUsuario jsonFilePath)



main :: IO()
main = do
    salvarUsuario "./dados.json" 1000 "Iris" "Iago"
    putStrLn (show (getUsuario "./dados.json"))
    removerUsuario "./dados.json" 1
    putStrLn (show (getUsuario "./dados.json"))
    putStrLn (show (getUsarioPorID 6 (getUsuario "./dados.json")))
