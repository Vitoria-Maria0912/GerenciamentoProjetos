module Haskell.FuncoesAuxiliares where
import Haskell.Usuario (Usuario)

 -- função para verificar se um projeto com certo nome já está cadastrado
 verificaNomeProjeto :: String -> Bool
 verificaNomeProjeto msg = True


 -- função para verificar se um nome já está sendo usado por um usuário
 verificaNomeUsuario :: String -> Bool
 verificaNomeUsuario msg = True


 -- função para verificar se uma senha já é usada por um usuário
 verificaSenhaUsuario :: String -> Bool
 verificaSenhaUsuario msg = True

-- função para verificar se um id existe
 verificaIdExistente :: Int -> [Usuario] -> Bool
 verificaIdExistente id usuarios = elem id (map Usuario.idUsuario usuarios)
