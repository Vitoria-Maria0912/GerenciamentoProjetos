module Haskell.Database.Database where

import System.IO
import System.Directory
import Control.Monad (filterM)
import Haskell.Util.AbrirFecharArquivo



-- Função que retorna o local padrão dos users criados
diretorioDatabase :: String
diretorioDatabase = "./Modules/Database/LocalUsers/"



-- cria usuario na base de dados
criaUsuarioDatabase :: String -> String -> IO()
criaUsuarioDatabase nome senha = do
    let usuario = [nome, senha] -- usuario é uma lista que guarda os parâmetros passados
    createDirectory (diretorioDatabase ++ nome)
    createDirectory (diretorioDatabase ++ "/" ++ nome ++ "/" ++ "listas")
    createDirectory (diretorioDatabase ++ "/" ++ nome ++ "/" ++ "sharedWithMe")
    withFile (diretorioDatabase ++ nome ++ "/" ++ nome ++ ".txt") WriteMode $ \handle -> do
        hPutStrLn handle (unlines usuario)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username



-- Função para deletar um user da base de dados
-- usa uma função de deletar um arquivo passando o caminho do arquivo
deletaUsuarioDatabase :: String -> IO()
deletaUsuarioDatabase nomeUsuario = do 
    removeFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++".txt") 



-- Função que retorna o nome de um usuário
-- o termo 'conteudo' recebe os dados lidos no txt
pegaNomeDatabase :: String -> IO String
pegaNomeDatabase nomeUsuario = do 
    conteudo <- readFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++ ".txt")
    let linhas = lines conteudo 
    return (linhas !! 1)



-- Adiciona projeto na base de dados
addProjetoDatabase :: String -> String -> String -> String -> IO()
addProjetoDatabase idProjeto nomeProjeto descricao gerente = do
    let taskcontent = [idProjeto, nomeProjeto, descricao, gerente]
    let filePath = diretorioDatabase++nomeProjeto++"/idProjeto"++idProjeto++"/"
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines taskcontent)


-- Adiciona tarefa na base de dados
addAtividadeDatabase :: String -> String -> String -> String -> String -> IO()
addAtividadeDatabase idTarefa nomeTarefa descricaoTarefa statusTarefa membroResponsavel = do
    let taskcontent = [idTarefa, nomeTarefa, descricaoTarefa, statusTarefa, membroResponsavel]
    let filePath = diretorioDatabase++nomeTarefa++"/idTarefa/"++idTarefa++"/"++nomeTarefa
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines taskcontent)


-- Remove projeto da base de dados
removeProjetoDatabase :: String -> String -> IO()
removeProjetoDatabase idProjeto nomeProjeto = do
    let filePath = diretorioDatabase++nomeProjeto++"/idProjeto/"++idProjeto++"/"++nomeProjeto
    removeFile (filePath ++ nomeProjeto)



-- exibe Tarefas
exibeTarefasDatabase :: String -> String -> String -> IO [String]
exibeTarefasDatabase nomeTarefa descricaoTarefa idTarefa = do
    let filePath = diretorioDatabase++nomeTarefa++"/idTarefa/"++idTarefa++"/"++nomeTarefa
    conteudo <- readFile filePath
    let linhas = lines conteudo
    return linhas



-- remove tarefa
deleteTaskDatabase :: String -> String -> IO()
deleteTaskDatabase nomeTarefa idTarefa = do
    let filePath = diretorioDatabase++nomeTarefa++"/idTarefa/"++idTarefa++"/"++nomeTarefa
    removeFile (filePath ++ nomeTarefa)