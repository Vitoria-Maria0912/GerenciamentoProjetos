-- module Database.Database where

-- import System.IO
-- import System.Directory
-- import Control.Monad (filterM)
-- import Util.AbrirFecharArquivo


-- Função para deletar um usuário da base de dados
-- usa uma função de deletar um arquivo passando o caminho do arquivo
-- deletaUsuarioDatabase :: String -> IO()
-- deletaUsuarioDatabase nomeUsuario = do 
--    removeFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++".txt") 

-- Retorna o nome de um usuário
-- 'conteudo' recebe os dados lidos no txt
-- getNomeDatabase :: String -> IO String
-- getNomeDatabase nomeUsuario = do 
--    conteudo <- readFile (diretorioDatabase++nomeUsuario++"/"++nomeUsuario++ ".txt")
--    let linhas = lines conteudo 
--    return (linhas !! 1)


-- <<< PROJETOS >>>


-- Remove projeto da base de dados
-- removeProjetoDatabase :: String -> IO ()
-- removeProjetoDatabase idProjeto = do
--    let filePath = diretorioDatabaseProjetos ++ idProjeto
--    removeDirectoryRecursive filePath 


-- <<< ATIVIDADES >>>

-- Remove atividade da base de dados
-- removeAtividadeDatabase :: String -> IO()
-- removeAtividadeDatabase idAtividade = do
--    let filePath = diretorioDatabaseAtividades ++ idAtividade
--    removeFile (filePath ++ idAtividade)
