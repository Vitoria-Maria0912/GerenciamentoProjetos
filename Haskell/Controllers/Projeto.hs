module Haskell.Controllers.Projeto where

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Haskell.Controllers.Atividades as Atividade (Atividade)
import Haskell.Database.Database


data Projeto = Projeto {
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    gerenteNome :: String,
    atividadesProjeto :: [Atividade]
} 


-- criação de projeto
criaProjeto :: String -> String -> String -> String -> IO()
criaProjeto = addProjetoDatabase


-- remoção de projeto
removeProjeto :: String -> IO()
removeProjeto = removeProjetoDatabase
