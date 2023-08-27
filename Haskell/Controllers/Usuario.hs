module Haskell.Controllers.Usuario where
import System.IO
import Haskell.Database.Database -- import Database para realizar as funções


-- Definindo o tipo de dado que é USER, quais parâmetros recebe
data Usuario = User { nome :: String, 
 password :: String
}


criaUsuario :: String -> String -> IO()
criaUsuario = criaUsuarioDatabase -- Função que cria novos Users, executada em Database
