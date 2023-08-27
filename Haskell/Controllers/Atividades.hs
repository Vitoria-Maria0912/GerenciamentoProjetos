module Haskell.Controllers.Atividades where
import Haskell.Controllers.Usuario as Usuario ( Usuario )


data Atividade = Atividade {
    idAtividade :: Int,
    tituloAtividade :: String,
    descricaoAtividade :: String,
    statusAtividade :: String,
    membroResponsavel :: Maybe Usuario
} 


