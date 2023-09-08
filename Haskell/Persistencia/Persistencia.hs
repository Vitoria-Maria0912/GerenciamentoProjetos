module Persistencia.Persistencia where

import System.IO
import System.Directory
import Controllers.Atividades as Atividades

-- Função para salvar uma atividade em um arquivo de texto
salvarAtividade :: Atividade -> IO ()
salvarAtividade atividade = do
    let filePath = "./database/atividades/" ++ idAtividade atividade ++ ".txt"
    withFile filePath WriteMode $ \file -> do
        hPutStrLn file (titulo atividade)
        hPutStrLn file (descricao atividade)
        hPutStrLn file (idProjetoAtividade atividade)
        hPutStrLn file (idAtividade atividade)
        hPutStrLn file (status atividade)
        case idMembroResponsavel atividade of
            Just membro -> hPutStrLn file membro
            Nothing -> hPutStrLn file "Nothing"
        case feedback atividade of
            Just fb -> hPutStrLn file (unlines fb)
            Nothing -> hPutStrLn file "Nothing"

-- Função para carregar uma atividade de um arquivo de texto
carregarAtividade :: String -> IO (Maybe Atividade)
carregarAtividade idAtividade = do
    let filePath = "./database/atividades/" ++ idAtividade ++ ".txt"
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            conteudo <- readFile filePath
            let linhas = lines conteudo
            let titulo' = linhas !! 0
            let descricao' = linhas !! 1
            let idProjeto' = linhas !! 2
            let idAtividade' = linhas !! 3
            let status' = linhas !! 4
            let idMembro' = if linhas !! 5 == "Nothing" then Nothing else Just (linhas !! 5)
            let feedback' = if linhas !! 6 == "Nothing" then Nothing else Just (drop 1 $ linhas !! 6)
            let atividade = Atividade titulo' descricao' idProjeto' idAtividade' status' idMembro' feedback'
            return (Just atividade)
        else return Nothing
