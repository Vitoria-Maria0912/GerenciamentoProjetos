module Haskell.Controllers.Usuario where
import System.IO
import Haskell.Database.Database -- import Database para realizar as funções

data User = User { username :: String,
 name :: String, 
 password :: String,
 description :: String }
-- Definindo o tipo de dado que é USER, quais parâmetros recebe

createUser :: String -> String -> String -> String -> IO()
createUser = createUserDatabase -- Função que cria novos Users, executada em Database

deleteUser :: String -> IO()
deleteUser = deleteUserDatabase -- Função que deleta um User, executada em Database

getName :: String -> IO String
getName = getNameDatabase -- Função que retorna o Nome de um User, resgata do Database

getDescription :: String -> IO String
getDescription = getDescriptionDatabase -- Função que retorna a Descrição de um User, resgata do Database

loginUser :: String -> String -> IO Bool
loginUser = loginDatabase -- Função que realiza o login de um user, fazendo contato também com o Database

editUser :: String -> String -> String -> String -> IO()
editUser = editUserDatabase -- Função que edita um User, executada em Database