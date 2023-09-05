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
criaUsuario :: Int -> String -> String -> IO()
criaUsuario = criaUsuarioDatabase-- executada em Database


-- remoção de usuário
removeUsuario :: Int -> IO()
removeUsuario = deletaUsuarioDatabase -- executada em Database


-- verifica se o Id existe na lista de usuarios (usando o valor de idUsuarios linkado ao Usuario)
verificaIdUsuario :: Int -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)
