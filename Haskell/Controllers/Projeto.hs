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
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: Int,
    atividadesProjeto :: [Atividade]
} 


-- criação de projeto
criaProjeto :: String -> String -> String -> String -> IO()
criaProjeto = addProjetoDatabase


adicionaProjeto :: Projeto -> [Projeto] -> [Projeto]
adicionaProjeto projeto projetos = 
    case find (\u -> nomeProjeto u == nomeProjeto projeto) projetos of 
        Just _-> projetos
        Nothing -> projeto : projetos


-- remoção de projeto
removeProjeto :: String -> IO()
removeProjeto = removeProjetoDatabase