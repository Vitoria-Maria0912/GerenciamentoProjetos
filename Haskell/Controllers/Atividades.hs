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


-- YALLE FEZ:



-- (NÃO COMPILA)
-- le as atividades do txt e retorna a lista
-- lerAtividades :: FilePath -> IO [Atividade]
-- lerAtividades path = do
--     conteudo <- readFile path
--     let atividades = mapMaybe fromStringAtv $ lines conteudo
--     return atividades
  
-- (NÃO COMPILA)
-- converte uma string em um objeto do tipo Atividade
-- fromStringAtv :: String -> Maybe Atividade
-- fromStringAtv str = case words str of
--   [titulo, descricao, status, idProjetoAtividade, idAtividade, idMembroResponsavel, feedback] -> do
--     return Atividade {titulo = titulo,
--                       descricao = descricao, 
--                       status = status, 
--                       idProjetoAtividade = idProjetoAtividade, 
--                       idAtividade = idAtividade, 
--                       idMembroResponsavel = idMembroResponsavel, 
--                       feedback = feedback}
--   _ -> Nothing






