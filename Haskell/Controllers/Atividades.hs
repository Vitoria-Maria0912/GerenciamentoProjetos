module Controllers.Atividades where

import Data.List (find)
import Controllers.Usuario
import Database.Database


data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    idProjeto :: String, 
    idAtividade :: String,
    status :: String,
    membroResponsavel :: Maybe Usuario
} deriving ()


criarAtividade :: String -> String -> String -> String -> String -> Maybe Usuario -> Atividade
criarAtividade titulo descricao status idProjeto idAtividade membroResponsavel =
    Atividade { titulo = titulo, 
                descricao = descricao, 
                idProjeto = idProjeto,
                idAtividade = idAtividade,
                status = status, 
                membroResponsavel = membroResponsavel }
    

adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
adicionaAtividade atividade atividades = 
    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
        Just _-> atividades
        Nothing -> atividade : atividades


getAtividades :: String -> [Atividade] -> Maybe [String] 
getAtividades id atividades =
    case filter (\u -> idAtividade u == id) atividades of
        [atividadeEncontrada] -> Just (formataAtividade atividadeEncontrada)
        _ -> Nothing


formataAtividade:: Atividade -> [String]
formataAtividade atividade = 
    ["Titulo: " ++ titulo atividade ++ "\n" ++
    "Descrição: " ++ descricao atividade ++ "\n" ++
    "ID Projeto: " ++ idProjeto atividade ++ "\n" ++
    "ID Atividade: " ++ idAtividade atividade ++ "\n" ++
    "Status: " ++ status atividade ++ "\n"]
    

mudaStatus :: Atividade -> String -> Atividade
mudaStatus atividade novoStatus = atividade {status = novoStatus}


removerAtividade :: String -> IO()
removerAtividade = removeAtividadeDatabase


