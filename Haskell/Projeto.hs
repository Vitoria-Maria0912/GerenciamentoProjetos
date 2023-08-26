module Projeto where 

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Projeto = Projeto {
    idProjeto :: Int, nome :: String , descricao :: String, senha :: String} deriving (Show, Read, Eq)

cadastraProjeto :: Int -> String -> String -> String -> Project
cadastraProjeto idProject nome descricao senha = (Project {idProject = idProject, nome = nome, descricao = descricao})

--finalizar depois *fazer a checagem novamente antes de adicionar ao "TXT" ?
adicionaProjeto :: Project -> [Project] -> [Project]

escreverProjeto :: FilePath -> [Project] -> IO ()
escreverUsuarios arquivo projects = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarProjeto usuarios
    formatarProjeto p = "ID: " ++ show (idProjeto p) ++ ", NOME: " ++ nome p ++ ", DESCRICAO: " ++ descricao p ++ ", SENHA:" ++ senha p
