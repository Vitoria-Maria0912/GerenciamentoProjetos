{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Usuario where
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.IO.Unsafe
import System.Directory

instance FromJSON Usuario
instance ToJSON Usuario


-- | Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: Int,
    nome :: String,
    senha :: String
} deriving (Show, Generic)


-- | Função que retorna os dados de um usuário de acordo com o seu ID
getUsuario:: Int-> [Usuario] -> Maybe Usuario
getUsuario _ [] = Nothing
getUsuario usuarioId (x:xs)     
  | idUsuario x == usuarioId = Just x
  | otherwise = getUsuario usuarioId xs

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
  let usuario = Usuario idUsuario nome senha
  let userList = (getUsuarios jsonFilePath) ++ [usuario]

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

-- | Função que verifica se a senha pertence ao usuário
verificaSenhaUsuario :: Usuario -> String -> Bool
verificaSenhaUsuario usuario senhaUsuario = ((senha usuario) == senhaUsuario)

-- | Função que retorna o numero de usuarios atuais do sistema (de acordo com o arquivo)
getNumDeUsuarios :: String -> Int
getNumDeUsuarios jsonFilePath = length (getUsuarios jsonFilePath)
