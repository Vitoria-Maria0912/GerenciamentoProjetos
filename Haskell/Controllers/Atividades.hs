module Controllers.Atividades where
import Controllers.Usuario


data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    idAtividade :: Int,
    idProjeto :: Int, 
    status :: String,
    membroResponsavel :: Maybe Usuario
} deriving ()


criaAtividade :: String -> String -> String -> Int -> Int -> Maybe Usuario -> Atividade
criaAtividade titulo descricao status idAtividade idProjeto membroResponsavel = Atividade { titulo = titulo, 
                                                                                            descricao = descricao, 
                                                                                            idAtividade = idAtividade,
                                                                                            idProjeto = idProjeto,
                                                                                            status = status, 
                                                                                            membroResponsavel = membroResponsavel }
    


