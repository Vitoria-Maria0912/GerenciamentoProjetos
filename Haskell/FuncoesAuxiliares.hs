module Haskell.FuncoesAuxiliares where
import System.Directory (doesFileExist)
import qualified Data.Text.IO as TIO
import Data.Text (unpack)


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


-- Função para gerar um ID aleatório
geraIDAleatorio ::  Int
geraIDAleatorio = 0


-- Função para gerar um ID de usuário aleatório
geraIDUsuario :: Int
geraIDUsuario = geraIDAleatorio


-- Função para gerar um ID de atividade aleatório
geraIDAtividade :: Int
geraIDAtividade = geraIDAleatorio


-- Função para gerar um ID de projeto aleatório
geraIDProjeto :: Int
geraIDProjeto = geraIDAleatorio

