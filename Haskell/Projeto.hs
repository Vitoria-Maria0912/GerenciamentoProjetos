module Project where 

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Project = Project {
    idProject :: Int, nome :: String , descricao :: String, senha :: String} deriving (Show, Read, Eq)

cadastraProject :: Int -> String -> String -> String -> Project
cadastraProject idProject nome descricao senha = (Project {idProject = idProject, nome = nome, descricao = descricao})

--finalizar depois *fazer a checagem novamente antes de adicionar ao "TXT" ?
adicionaProject :: Project -> [Project] -> [Project]

escreverProjects :: FilePath -> [Project] -> IO ()
escreverUsuarios arquivo projects = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarProject usuarios
    formatarProject p = "ID: " ++ show (idProject p) ++ ", NOME: " ++ nome p ++ ", DESCRICAO: " ++ descricao p ++ ", SENHA:" ++ senha p