module Haskell.Util.Util where

import Data.Map as Map (fromList, Map)
import Haskell.Controllers.Usuario as Usuario (Usuario)
import Haskell.Controllers.Projeto as Projeto (Projeto)
import Data.Maybe (mapMaybe)


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
