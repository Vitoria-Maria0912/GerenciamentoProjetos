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
    membroResponsavel :: Maybe Usuario,
    feedback :: Maybe [String]
} 

criarAtividade :: String -> String -> String -> String -> String -> Maybe Usuario -> Maybe [String] -> Atividade
criarAtividade titulo descricao status idProjetoAtividade idAtividade membroResponsavel feedback =
    Atividade { titulo = titulo, 
                descricao = descricao, 
                idProjetoAtividade = idProjetoAtividade,
                idAtividade = idAtividade,
                status = status, 
                membroResponsavel = membroResponsavel,
                feedback = feedback }
    
-- Adiciona uma atividade no sistema
adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
adicionaAtividade atividade atividades = 
    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
        Just _-> atividades
        Nothing -> atividade : atividades

-- Exibe uma Atividade, em formato de lista com todos os seus atributos
getAtividades :: String -> [Atividade] -> Maybe [String] 
getAtividades id atividades =
    case filter (\u -> idAtividade u == id) atividades of
        [atividadeEncontrada] -> Just (formataAtividade atividadeEncontrada)
        _ -> Nothing

-- Formata a atividade em uma lista com todos os seus atributos
formataAtividade:: Atividade -> [String]
formataAtividade atividade = 
    ["Titulo: " ++ titulo atividade ++ "\n" ++
    "Descrição: " ++ descricao atividade ++ "\n" ++
    "ID Projeto: " ++ idProjetoAtividade atividade ++ "\n" ++
    "ID Atividade: " ++ idAtividade atividade ++ "\n" ++
    "Status: " ++ status atividade ++ "\n"]
    
-- Muda o status de uma atividade
mudaStatus :: Atividade -> String -> Atividade
mudaStatus atividade novoStatus = atividade {status = novoStatus}

-- Adiciona um Feedback a uma atividade
adicionaFeedback :: Atividade -> String -> Maybe [String]
adicionaFeedback atividade comentario = do
    let maybeFeedback = feedback atividade
    case maybeFeedback of
        Just feedbacksAtuais -> Just (comentario : feedbacksAtuais)
        Nothing -> Just [comentario]

-- Remove uma atividade do sistema
removerAtividade :: String -> IO()
removerAtividade = removeAtividadeDatabase


