module Haskell.Controllers.Usuario where
import System.IO
import Haskell.Database.Database -- import Database para realizar as funções


-- Definindo o tipo de dado que é USER, quais parâmetros recebe (id, nome, senha)
data Usuario = Usuario { 
 idUsuario :: Int
 nome :: String,
 password :: String
} deriving (Show) -- para mostrar o usuario como string

-- criação de usuário
criaUsuario :: Int -> String -> String -> IO()
criaUsuario = criaUsuarioDatabase-- executada em Database

-- remoção de usuário
removeUsuario :: Int -> IO()
removeUsuario = deletaUsuarioDatabase -- executada em Database
