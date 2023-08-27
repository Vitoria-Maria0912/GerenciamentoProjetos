module Haskell.Projeto where

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Haskell.Atividades (Atividade)


data Projeto = Projeto {
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    atividadesProjeto :: [Atividade]
} deriving (Show)


-- cadastra um Projeto
cadastraProjeto :: Int -> String -> String -> String -> Projeto
cadastraProjeto idProjeto nome descricao senha = (Projeto {idProjeto = idProjeto, 
nomeProjeto = nome, descricaoProjeto = descricao, atividadesProjeto = []})



--finalizar depois *fazer a checagem novamente antes de adicionar ao "TXT" ?
-- adicionaProjeto :: Project -> [Project] -> [Project]


-- Escreve projeto no arquivo .txt
{-
escreverProjeto :: FilePath -> [Projeto] -> IO ()
escreverProjeto arquivo projects = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarProjeto usuarios
    formatarProjeto p = "ID: " ++ show (idProjeto p) ++ ", NOME: " ++ nome p ++ ", DESCRICAO: " ++ descricao p ++ ", SENHA:" ++ senha p
-}


-- Falta remover Projeto --


-- Adiciona atividades ao projeto
adicionaAtividade :: Atividade -> Projeto -> Projeto
adicionaAtividade atividade projeto =
    projeto { atividadesProjeto = atividadesProjeto projeto ++ [atividade] }


-- Função para remover uma atividade de um projeto
removerAtividade :: Projeto -> String -> Projeto
removerAtividade projeto tituloAtividade =
    projeto { atividadesProjeto = filter (\atividade -> tituloAtividade /= tituloAtividade) (atividadesProjeto projeto) }



-- Exibe todas as atividades do projeto
exibeAtividades :: Projeto -> [Atividade]
exibeAtividades = atividadesProjeto
