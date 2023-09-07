module Controllers.Projeto where

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Controllers.Atividades as Atividade
import Database.Database


data Projeto = Projeto {
    idProjeto :: String,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: String,
    --como só contém os ID dos usuarios , não precisa ser necessariamente uma lista do tipo Usuario .
    usuarios :: [String],
    -- como só contém os ID das atividades , não precisa ser uma lista do tipo atividades , só podendo ter o ID .
    atividades :: [String]
} deriving ()


-- criação de projeto
criaProjeto :: String -> String -> String -> String -> IO()
criaProjeto = addProjetoDatabase


-- adiciona um Projeto a uma lista de Projetos /recebe um projeto ,uma lista de projetos e retorna a lista atualizada 
-- checagem conforme nome do projeto
adicionaProjeto :: Projeto -> [Projeto] -> [Projeto]
adicionaProjeto projeto projetos = 
    case find (\u -> nomeProjeto u == nomeProjeto projeto) projetos of 
        Just _-> projetos
        Nothing -> projeto : projetos

-- remoção de projeto
removeProjeto :: String -> IO()
removeProjeto = removeProjetoDatabase