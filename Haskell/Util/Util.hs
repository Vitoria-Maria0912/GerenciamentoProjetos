module Util.Util where

import Data.Map as Map (fromList, Map)
import Controllers.Usuario as Usuario
import Controllers.Projeto as Projeto
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)


-- verifica se o Id existe na lista de usuarios (usando o valor de idUsuarios linkado ao Usuario)  (ver se precisa colocar na Database)
verificaIdUsuario :: String -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)


-- formata em string dados de um usuario  (ver se precisa colocar na Database)
formataUsuario:: Usuario -> String
formataUsuario usuario = 
   "ID: " ++ show (idUsuario usuario) ++ "\n" ++
   "Nome: " ++ nome usuario ++ "\n"


-- checa se o usuario é gerente de algum projeto da lista de projetos. 
ehGerente :: Int -> [Projeto] -> Bool
ehGerente id gerentes = any (\projeto -> id == idGerente projeto) gerentes


--dar Upper Case para salvamento de dados afim de evitar na verificaçãoNomeProjeto possivel erro ou geração de duplicata.
toUpperCase :: String -> String
toUpperCase str = map toUpper str