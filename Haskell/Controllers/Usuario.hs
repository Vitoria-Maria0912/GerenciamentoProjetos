module Controllers.Usuario where
import System.IO
import Database.Database -- import Database para realizar as funções


-- Definindo o tipo de dado que é USER, quais parâmetros recebe
data Usuario = Usuario { nome :: String,
 password :: String
}

-- criação de usuário
criaUsuario :: String -> String -> IO()
criaUsuario = criaUsuarioDatabase-- executada em Database


-- remoção de usuário
removeUsuario :: String -> IO()
removeUsuario = deletaUsuarioDatabase -- executada em Database