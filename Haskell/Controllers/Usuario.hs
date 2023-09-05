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
geraIdUsuario :: [Usuario] -> Int
geraIdUsuario usuarios = length usuarios



-- verifica se o Id existe na lista de usuarios (usando o valor de idUsuarios linkado ao Usuario)
-- Caso o valor do Id ja exista e ele for menor que o tamanho da lista (caaso onde houve remoção de usuario), adicionar mais um quando gerar o ID
verificaIdUsuario :: Int -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)
