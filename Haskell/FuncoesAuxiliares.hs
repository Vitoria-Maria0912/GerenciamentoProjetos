module Haskell.FuncoesAuxiliares where
<<<<<<< HEAD
import System.Directory (doesFileExist)
import qualified Data.Text.IO as TIO
import Data.Text (unpack)
=======
import Haskell.Usuario (Usuario)

 -- função para verificar se um projeto com certo nome já está cadastrado
 verificaNomeProjeto :: String -> Bool
 verificaNomeProjeto msg = True
>>>>>>> deb2b62d5900139f85f7681efbdcee4fae4c75c8



-- Função para verificar se um nome de usuário já está sendo usado
verificaNomeUsuario :: String -> IO Bool
verificaNomeUsuario nome = do
    arquivoExiste <- doesFileExist "usuarios.txt"
    if not arquivoExiste
        then return False
    else do
        conteudo <- TIO.readFile "usuarios.txt"
        let usuarios = lines (unpack conteudo)
            nomes = map (takeWhile (/= ',')) usuarios
        return $ elem nome nomes


<<<<<<< HEAD

-- Função para verificar se um nome de projeto já está sendo usado
verificaNomeProjeto :: String -> IO Bool
verificaNomeProjeto nome = do
    arquivoExiste <- doesFileExist "projetos.txt"
    if not arquivoExiste
        then return False
    else do
        conteudo <- TIO.readFile "projetos.txt"
        let projetos = lines (unpack conteudo)
            nomes = map (takeWhile (/= ',')) projetos
        return $ elem nome nomes



-- Função para verificar se uma senha já está sendo usada
verificaSenhaUsuario :: String -> IO Bool
verificaSenhaUsuario senha = do
    arquivoExiste <- doesFileExist "usuarios.txt"
    if not arquivoExiste
        then return False
    else do
        conteudo <- TIO.readFile "usuarios.txt"
        let usuarios = lines (unpack conteudo)
            senhas = map (dropWhile (/= ',')) usuarios
        return $ elem senha senhas
=======
 -- função para verificar se uma senha já é usada por um usuário
 verificaSenhaUsuario :: String -> Bool
 verificaSenhaUsuario msg = True

-- função para verificar se um id existe
 verificaIdExistente :: Int -> [Usuario] -> Bool
 verificaIdExistente id usuarios = elem id (map Usuario.idUsuario usuarios)
>>>>>>> deb2b62d5900139f85f7681efbdcee4fae4c75c8
