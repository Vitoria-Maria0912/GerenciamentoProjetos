module Haskell.Database.Database where

import System.IO
import System.Directory
import Control.Monad (filterM)
import Haskell.Util.AbrirFecharArquivo


-- Função que retorna o local padrão dos users criados
diretorioDatabase :: String
diretorioDatabase = "./Modules/Database/LocalUsers/"


criaUsuarioDatabase :: String -> String -> IO()
criaUsuarioDatabase nome senha = do
    let usuario = [nome, senha] -- usuario é uma lista que guarda os parâmetros passados
    createDirectory (diretorioDatabase ++ nome)
    createDirectory (diretorioDatabase ++ "/" ++ nome ++ "/" ++ "listas")
    createDirectory (diretorioDatabase ++ "/" ++ nome ++ "/" ++ "sharedWithMe")
    withFile (diretorioDatabase ++ nome ++ "/" ++ nome ++ ".txt") WriteMode $ \handle -> do
        hPutStrLn handle (unlines usuario)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username
    