{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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


-- | Função que retorna os dados de um usuário de acordo com o seu ID
getUsario:: Int-> [Usuario] -> Maybe Usuario
getUsario _ [] = Nothing
getUsario id (x:xs)     
  | idUsuario x == id = Just x
  | otherwise = getUsario id xs

-- | Função que retorna a lista atual de usuários cadastrados no sistema lendo o arquivo.
getUsuarios :: String -> [Usuario]
getUsuarios path = do
 let file = unsafePerformIO( B.readFile path )
 let decodedFile = decode file :: Maybe [Usuario]
 case decodedFile of
  Nothing -> []
  Just usuarios -> usuarios

-- | Função que imprime o usuário omitindo informação sensível
imprimirUsuario :: Usuario -> IO()
imprimirUsuario u = putStrLn $ "ID: " ++ show (idUsuario u) ++ ", Nome: " ++ nome u

-- | Função que salva e escreve o usuario no arquivo diretamente
salvarUsuario :: String -> Int -> String -> String -> IO()
salvarUsuario jsonFilePath idUsuario nome senha = do
 let novoId = (length (getUsuarios jsonFilePath)) + 1
 let u = Usuario novoId nome senha
 let userList = (getUsuarios jsonFilePath) ++ [u]

 B.writeFile "../Temp.json" $ encode userList
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath

-- | Função que remove usuario pelo ID
removeUsarioPorID :: Int -> [Usuario] -> [Usuario]
removeUsarioPorID _ [] = []
removeUsarioPorID idUsuarioS(x:xs)
 | (idUsuario x) == idUsuarioS = xs
 | otherwise = [x] ++ (removeUsarioPorID idUsuarioS xs)

-- | Função que remove usuario da lista de usuarios reescrevendo o arquivo.
removerUsuario :: String -> Int -> IO()
removerUsuario jsonFilePath idUsuario = do
 let usuarios = getUsuarios jsonFilePath
 let novosUsuarios = removeUsarioPorID idUsuario usuarios

 B.writeFile "../Temp.json" $ encode novosUsuarios
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath



main :: IO()
main = do
    --salvarUsuario "./dados.json" 1000 "Iris" "Iago"
    --salvarUsuario "./dados.json" 1750 "Yalle" "tuts"
    --putStrLn (show (getUsuario "./dados.json"))
   -- removerUsuario "./dados.json" 1
  let usuarios = getUsuarios "./dados.json"
  mapM_ imprimirUsuario usuarios 
    -- putStrLn (show (getUsarioPorID 2 (getUsuario "./dados.json")))
   -- salvarUsuario "./dados.json" 10 "Vitoria" "ruiva"
   -- putStrLn (show (getUsuario "./dados.json"))
   -- putStrLn (show (getNumDeUsuarios "./dados.json"))
