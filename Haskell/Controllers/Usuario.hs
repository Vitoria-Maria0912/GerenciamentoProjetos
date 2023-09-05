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

-- verifica se o Id existe na lista de usuarios (usando o valor de idUsuarios linkado ao Usuario)  (ver se precisa colocar na Database)
verificaIdUsuario :: Int -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)

-- retorna a representacao do usuario em string (ver se precisa colocar na Database)
getUsuario:: Int -> [Usuario] -> Maybe String
getUsuario id usuarios =
  case filter (\u -> idUsuario u == id) usuarios of [usuarioEncontrado] -> Just (formataUsuario usuarioEncontrado) 
  _ -> Nothing

-- formata em string dados de um usuario  (ver se precisa colocar na Database)
formataUsuario:: Usuario -> String
formataUsuario usuario = 
   "ID: " show (idUsuario usuario) ++ "\n" ++
   "Nome: " nome usuario ++ "\n"



