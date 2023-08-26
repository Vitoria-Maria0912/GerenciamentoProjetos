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


criaAtividade :: String -> String -> Atividade
criaAtividade titulo descricao = Atividade titulo descricao NaoAtribuida Nothing


atribuiMembro :: Atividade -> Usuario -> Atividade
atribuiMembro atividade usuario = atividade { membroResponsavel = Just usuario }


mudaStatus :: StatusAtividade -> Atividade -> Atividade
mudaStatus novoStatus atividade = atividade { statusAtividade = novoStatus }


removeAtividade :: Projeto -> String -> Projeto
removeAtividade projeto titulo = projeto { atividadesProjeto =
     filter (\atividade -> tituloAtividade atividade /= titulo) (atividadesProjeto projeto) }
