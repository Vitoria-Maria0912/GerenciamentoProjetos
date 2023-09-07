module Controllers.Atividades where
import Controllers.Usuario
import Database.Database

data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    idAtividade :: String,
    idProjeto :: String, 
    status :: String,
    membroResponsavel :: Maybe Usuario
} deriving ()


criarAtividade :: String -> String -> String -> String -> String -> Maybe Usuario -> Atividade
criarAtividade titulo descricao status idAtividade idProjeto membroResponsavel =
    Atividade { titulo = titulo, 
                descricao = descricao, 
                idAtividade = idAtividade,
                idProjeto = idProjeto,
                status = status, 
                membroResponsavel = membroResponsavel }
    

removerAtividade :: String -> IO()
removerAtividade = removeAtividadeDatabase


