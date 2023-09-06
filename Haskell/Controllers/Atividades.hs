module Controllers.Atividades where
import Controllers.Usuario as Usuario ( Usuario )


data Atividade = Atividade {
    idAtividade :: Int,
    tituloAtividade :: String,
    descricaoAtividade :: String,
    statusAtividade :: String,
    membroResponsavel :: Maybe Usuario
} 


