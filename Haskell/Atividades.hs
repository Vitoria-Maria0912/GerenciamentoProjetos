module Haskell.Atividades where
import qualified Haskell.Usuario as Usuario

data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    status :: String,
    membroResponsavel :: Usuario.Usuario
}
-- removerAtividade
