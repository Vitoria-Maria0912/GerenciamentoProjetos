module Haskell.Atividades where

import Haskell.Usuario ( Usuario )
import Haskell.Projeto ( Projeto )

data StatusAtividade = Concluida | Pendente | NaoAtribuida deriving (Show, Eq)

data Atividade = Atividade {
    tituloAtividade :: String,
    descricaoAtividade :: String,
    statusAtividade :: StatusAtividade,
    membroResponsavel :: Maybe Usuario
} deriving (Show)


--Faltou a função de criar atividades


-- função que atribui uma atividade a um membro
atribuiMembro :: Atividade -> Usuario -> Atividade
atribuiMembro atividade usuario = atividade { membroResponsavel = Just usuario }


-- função para modificar o status da atividade
mudaStatus :: StatusAtividade -> Atividade -> Atividade
mudaStatus novoStatus atividade = atividade { statusAtividade = novoStatus }


-- função para remover a atividade
removeAtividade :: Projeto -> String -> Projeto
removeAtividade projeto titulo = projeto { atividadesProjeto =
     filter (\atividade -> tituloAtividade atividade /= titulo) (atividadesProjeto projeto) }
