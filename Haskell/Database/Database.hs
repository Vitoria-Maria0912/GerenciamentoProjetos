module Database.Database where

import System.IO
import System.Directory
import Control.Monad (filterM)
import Util.AbrirFecharArquivo

-- <<< DIRETÓRIOS >>>

-- Função que retorna o local padrão dos users criados
diretorioDatabase :: String
diretorioDatabase = "./Modules/Database/"

-- Função que retorna o local padrão dos usuários criados
diretorioDatabaseUsuario :: String
diretorioDatabaseUsuario = "./Modules/Database/LocalUsers/"

-- Função que retorna o local padrão dos users criados
diretorioDatabaseProjetos :: String
diretorioDatabaseProjetos = "./Modules/Database/LocalProjects/"

-- -- Função que retorna o local padrão dos users criados
diretorioDatabaseAtividades :: String
diretorioDatabaseAtividades = "./Modules/Database/LocalProjects/LocalTasks"


-- <<< USUÁRIOS >>>

-- Cria usuario na base de dados
criaUsuarioDatabase :: String -> String -> String -> IO()
criaUsuarioDatabase idUsuario nome senha = do
    let usuario = [idUsuario, nome, senha] -- usuario é uma lista que guarda os parâmetros passados
    createDirectory (diretorioDatabaseUsuario ++ idUsuario)
    createDirectory (diretorioDatabaseUsuario ++ "/" ++ idUsuario ++ "/" ++ "listas")
    createDirectory (diretorioDatabaseUsuario ++ "/" ++ idUsuario ++ "/" ++ "sharedWithMe")
    withFile (diretorioDatabaseUsuario ++ nome ++ "/" ++ idUsuario ++ ".txt") WriteMode $ \handle -> do
        hPutStrLn handle (unlines usuario)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username

-- Função para deletar um usuário da base de dados
-- usa uma função de deletar um arquivo passando o caminho do arquivo
deletaUsuarioDatabase :: String -> IO()
deletaUsuarioDatabase nomeUsuario = do 
    removeFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++".txt") 

-- Retorna o nome de um usuário
-- 'conteudo' recebe os dados lidos no txt
getNomeDatabase :: String -> IO String
getNomeDatabase nomeUsuario = do 
    conteudo <- readFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++ ".txt")
    let linhas = lines conteudo 
    return (linhas !! 1)


-- <<< PROJETOS >>>

-- Adiciona projeto na base de dados
addProjetoDatabase :: String -> String -> String -> String -> IO()
addProjetoDatabase idProjeto nomeProjeto descricao gerente = do
    let taskcontent = [idProjeto, nomeProjeto, descricao, gerente]
    let filePath = diretorioDatabaseProjetos ++ nomeProjeto ++ "/idProjeto" ++ idProjeto ++ "/"
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines taskcontent)

-- Remove projeto da base de dados
removeProjetoDatabase :: String -> IO ()
removeProjetoDatabase idProjeto = do
    let filePath = diretorioDatabaseProjetos ++ idProjeto
    removeDirectoryRecursive filePath 


-- <<< ATIVIDADES >>>

-- Cria atividade na base de dados
criaAtividadeDatabase ::  String -> String -> String -> String -> String -> Maybe String -> Maybe [String] -> IO() -- parâmetros errados
criaAtividadeDatabase titulo descricao status idProjetoAtividade idAtividade idMembroResponsavel feedback = do
    let atividade = [titulo, descricao, status, idProjetoAtividade, idAtividade, show(feedback)] 
    createDirectory (diretorioDatabaseAtividades ++ idAtividade)

-- Adiciona atividade na base de dados
addAtividadeDatabase :: String -> String -> String -> String -> String -> IO()
addAtividadeDatabase titulo descricao status idAtividade idProjeto = do
    let conteudo = [titulo, descricao, status, idAtividade, idProjeto]
    let filePath = diretorioDatabaseAtividades ++ titulo ++ "/idTarefa/" ++ idAtividade ++ "/" ++ titulo
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines conteudo)

-- Exibe atividade da base de dados
exibeAtividadeDatabase :: String -> String -> String -> IO [String]
exibeAtividadeDatabase titulo descricaoTarefa idTarefa = do
    let filePath = diretorioDatabaseAtividades ++ titulo ++ "/idTarefa/" ++ idTarefa ++ "/" ++ titulo
    conteudo <- readFile filePath
    let linhas = lines conteudo
    return linhas

-- Remove atividade da base de dados
removeAtividadeDatabase :: String -> IO()
removeAtividadeDatabase idAtividade = do
    let filePath = diretorioDatabaseAtividades ++ idAtividade
    removeFile (filePath ++ idAtividade)