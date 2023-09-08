module Controllers.Usuario where
import System.IO
import Database.Database 


-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: String,
    nome :: String,
    senha :: String
}

-- Criação de um Usuário
criaUsuario :: String -> String -> String -> IO()
criaUsuario = criaUsuarioDatabase-- executada em Database


-- Remoção de um Usuário, pelo ID
removeUsuario :: String -> IO()
removeUsuario = deletaUsuarioDatabase -- executada em Database