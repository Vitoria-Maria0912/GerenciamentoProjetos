module Haskell.Atividades where
import Haskell.Usuario ( Usuario )
import Haskell.Projeto ( Projeto )


data Atividade = Atividade {
    tituloAtividade :: String,
    descricaoAtividade :: String,
    statusAtividade :: String,
    membroResponsavel :: Maybe Usuario
} deriving (Show)



--Faltou a função de criar atividades



-- função que atribui uma atividade a um membro
atribuiMembro :: Atividade -> Usuario -> Atividade
atribuiMembro atividade usuario = atividade { membroResponsavel = Just usuario }


-- por default uma atividade é não atribuída
-- função para modificar o status da atividade
mudaStatus :: String -> Atividade
mudaStatus novoStatus atividade = atividade { statusAtividade = novoStatus }


mostraStatus :: Atividade -> String
mostraStatus atividade = statusAtividade


-- função para remover a atividade
removeAtividade :: Projeto -> String -> Projeto
removeAtividade projeto titulo = projeto { atividadesProjeto =
     filter (\atividade -> tituloAtividade atividade /= titulo) (atividadesProjeto projeto) }
