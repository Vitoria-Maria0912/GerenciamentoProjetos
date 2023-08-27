module Haskell.Controllers.Atividades where
import Haskell.Controllers.Usuario as Usuario ( Usuario )


data Atividade = Atividade {
    idAtividade :: Int,
    tituloAtividade :: String,
    descricaoAtividade :: String,
    statusAtividade :: String,
    membroResponsavel :: Maybe Usuario
} 


-- função que cria uma atividade
criaAtividade :: Int -> String -> String -> Atividade
criaAtividade id titulo descricao =
    Atividade { idAtividade = id, tituloAtividade = titulo, 
    descricaoAtividade = descricao, 
    statusAtividade = "Não Atribuída", membroResponsavel = Nothing }


-- Modifica o status da atividade
mudaStatus :: String -> Atividade -> Atividade
mudaStatus novoStatus atividade = atividade { statusAtividade = novoStatus }


-- Membro responsável pela atividade
atribuiMembro :: Atividade -> Usuario -> Atividade
atribuiMembro atividade usuario = atividade { membroResponsavel = Just usuario }


