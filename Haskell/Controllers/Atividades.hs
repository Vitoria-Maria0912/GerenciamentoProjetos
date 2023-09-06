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


criaAtividade :: String -> String -> String -> String -> String -> Maybe Usuario -> Atividade
criaAtividade titulo descricao status idAtividade idProjeto membroResponsavel =
     Atividade { titulo = titulo, 
                descricao = descricao, 
                idAtividade = idAtividade,
                idProjeto = idProjeto,
                status = status, 
                membroResponsavel = membroResponsavel }
    


removeAtividade :: String -> IO()
removeAtividade = removeAtividadeDatabase


