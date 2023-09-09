module Controllers.Atividades where

import Data.List (find)
import Controllers.Usuario
import Database.Database


data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    idProjetoAtividade :: String, 
    idAtividade :: String,
    status :: String,
    idMembroResponsavel :: Maybe String,
    feedback :: Maybe [String]
} 

criarAtividade :: String -> String -> String -> String -> String -> Maybe String -> Maybe [String] -> IO()
criarAtividade = criaAtividadeDatabase

-- usar esse depois e excluir o de cima:
-- criarAtividade :: String -> String -> String -> String -> String -> Maybe String -> Maybe [String] -> Atividade
-- criarAtividade titulo descricao status idProjetoAtividade idAtividade idMembroResponsavel feedback =
--    Atividade { titulo = titulo, 
--                descricao = descricao, 
--                idProjetoAtividade = idProjetoAtividade,
--                idAtividade = idAtividade,
--                status = status, 
--                idMembroResponsavel = idMembroResponsavel,
--                feedback = feedback }

-- Muda o status de uma atividade
mudaStatus :: Atividade -> String -> Atividade
mudaStatus atividade novoStatus = atividade {status = novoStatus}

-- Pega o ID do membro responsável pela atividade
getMembroResponsavel :: Atividade -> String
getMembroResponsavel atividade = do
    let membroResponsavel = idMembroResponsavel atividade
    case membroResponsavel of
        Just _ -> show (idMembroResponsavel atividade)
        _ -> "Não atribuído!"

-- Adiciona um Feedback a uma atividade
adicionaFeedback :: Atividade -> String -> Maybe [String]
adicionaFeedback atividade comentario = do
    let maybeFeedback = feedback atividade
    case maybeFeedback of
        Just feedbacksAtuais -> Just (comentario : feedbacksAtuais)
        Nothing -> Just [comentario]
    
-- Adiciona uma atividade no sistema (com base no id - nao compila ainda)
-- adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
-- adicionaAtividade atividade atividades = 
--    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
--        Just _-> atividades
--        Nothing -> atividade : atividades
        
-- escreve atividade no txt
-- escreverAtividades :: FilePath -> [Atividade] -> IO ()
-- escreverAtividades arquivo atividades = appendFile arquivo conteudo
--  where
--    conteudo = unlines $ map formatarAtividade atividades
--    formatarAtividade a = "ID: " ++ idAtividade a ++ ", TITULO: " ++ titulo a ++ ", DESCRIÇÃO: " ++ descricao a ++ ", ID PROJETO: " ++ idProjetoAtividade a ++ ", STATUS: " ++ status a ++ ", MEMBRO RESPONSÁVEL: " ++ membroResponsavel a

-- le as atividades do txt e retorna a lista
-- lerAtividades :: FilePath -> IO [Atividade]
-- lerAtividades path = do
--  conteudo <- readFile path
--  let atividades = mapMaybe fromStringAtv $ lines conteudo
--  return atividades
  
-- converte uma string em um objeto do tipo Atividade
-- fromStringAtv :: String -> Maybe Atividade
-- fromStringAtv str = case words str of
--  [titulo, descricao, status, idProjetoAtividade, idAtividade, idMembroResponsavel, feedback] -> do
--    return Atividade {
--      titulo = titulo,
--      descricao = descricao,
--      status = status,
--      idProjetoAtividade = idProjetoAtividade,
--      idAtividade = idAtividade,
--      idMembroResponsavel = idMembroResponsavel,
--      feedback = feedback
--    }
--  _ -> Nothing



-- Formata a atividade em uma lista com todos os seus atributos
-- formataAtividade:: Atividade -> [String]
-- formataAtividade atividade = 
--    ["Titulo: " ++ titulo atividade ++ "\n" ++
--    "Descrição: " ++ descricao atividade ++ "\n" ++
--    "ID Projeto: " ++ idProjetoAtividade atividade ++ "\n" ++
--    "ID Atividade: " ++ idAtividade atividade ++ "\n" ++
--    -- "ID Membro Responsável: " ++ (getMembroResponsavel (atividade)) ++ "\n" ++
--    "Status: " ++ status atividade ++ "\n"]
    

-- Remove uma atividade do sistema (comentado pra nao usar a database)
-- removerAtividade :: String -> IO()
-- removerAtividade = removeAtividadeDatabase


