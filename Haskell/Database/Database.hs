module Database.Database where

import System.IO
import System.Directory
import Control.Monad (filterM)
import Util.AbrirFecharArquivo


-- Função que retorna o local padrão dos users criados
diretorioDatabase :: String
diretorioDatabase = "./Modules/Database/LocalUsers/"


-- cria usuario na base de dados
criaUsuarioDatabase :: String -> String -> String -> IO()
criaUsuarioDatabase idUsuario nome senha = do
    let usuario = [idUsuario, nome, senha] -- usuario é uma lista que guarda os parâmetros passados
    createDirectory (diretorioDatabase ++ idUsuario)
    createDirectory (diretorioDatabase ++ "/" ++ idUsuario ++ "/" ++ "listas")
    createDirectory (diretorioDatabase ++ "/" ++ idUsuario ++ "/" ++ "sharedWithMe")
    withFile (diretorioDatabase ++ nome ++ "/" ++ idUsuario ++ ".txt") WriteMode $ \handle -> do
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


-- Remove projeto da base de dados
removeProjetoDatabase :: String -> IO ()
removeProjetoDatabase idProjeto = do
    let filePath = diretorioDatabase ++ idProjeto
    removeDirectoryRecursive filePath 


-- Adiciona tarefa na base de dados
addAtividadeDatabase :: String -> String -> String -> String -> String -> IO()
addAtividadeDatabase titulo descricao status idAtividade idProjeto = do
    let conteudo = [titulo, descricao, status, idAtividade, idProjeto]
    let filePath = diretorioDatabase ++ titulo ++ "/idTarefa/" ++ idAtividade ++ "/" ++ titulo
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines conteudo)


-- exibe Tarefas
exibeAtividadeDatabase :: String -> String -> String -> IO [String]
exibeAtividadeDatabase titulo descricaoTarefa idTarefa = do
    let filePath = diretorioDatabase ++ titulo ++ "/idTarefa/" ++ idTarefa ++ "/" ++ titulo
    conteudo <- readFile filePath
    let linhas = lines conteudo
    return linhas


-- remove tarefa
removeAtividadeDatabase :: String -> IO()
removeAtividadeDatabase idAtividade = do
    let filePath = diretorioDatabase ++ idAtividade
    removeFile (filePath ++ idAtividade)