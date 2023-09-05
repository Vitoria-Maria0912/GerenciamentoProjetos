module Haskell.Controllers.Usuario where
import System.IO
import Haskell.Database.Database -- import Database para realizar as funções


-- Definindo o tipo de dado que é USER, quais parâmetros recebe (id, nome, senha)
data Usuario = Usuario { 
 idUsuario :: Int
 nome :: String,
 password :: String
}

-- criação de usuário
criaUsuario :: String -> String -> IO()
criaUsuario = criaUsuarioDatabase-- executada em Database


-- remoção de usuário
removeUsuario :: String -> IO()
removeUsuario = deletaUsuarioDatabase -- executada em Database


-- gera o número de Id do usuário de acordo com seu local na lista 
geraIdUsuario :: [Usuario] -> IO()
geraIdUsuario usuarios = length usuarios
