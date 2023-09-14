{-# LANGUAGE DeriveGeneric #-} -- <<<<<<<<<<<<<<<<<<<<
module Controllers.Projeto where

-------------------------------------------
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory
import Data.List (find)

import Controllers.Usuario
import Controllers.Atividades

instance FromJSON Projeto
instance ToJSON Projeto
-------------------------------------------


-- Definindo o tipo de dado Projeto
data Projeto = Projeto {
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: Int,
    membros :: Maybe [Usuario],
    atividades :: Maybe [Atividade]
} deriving (Show, Generic) 


-- Cria um novo projeto e o adiciona ao arquivo JSON
criaProjeto :: String -> Int -> String -> String -> Int -> Maybe[Usuario] -> Maybe[Atividade] -> IO ()
criaProjeto jsonFilePath idProjeto nomeProjeto descricaoProjeto idGerente membros atividades = do
    let projeto = Projeto idProjeto nomeProjeto descricaoProjeto idGerente Nothing Nothing
    let projetosAtualizados = (getTodosProjetos jsonFilePath) ++ [projeto]

    B.writeFile "../Temp.json" $ encode projetosAtualizados
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath

-- Verifica se o usuário é gerente de algum projeto do sistema 
ehGerente :: Int -> [Projeto] -> Bool
ehGerente gerenteId gerentes = any (\projeto -> gerenteId == (idGerente projeto)) gerentes

-- Obtém um projeto a partir do ID
getProjeto :: Int -> [Projeto] -> Maybe Projeto
getProjeto _ [] = Nothing
getProjeto id (x:xs)
  | idProjeto x == id = Just x
  | otherwise = getProjeto id xs

-- Obtém todos os projetos cadastrados no sistema
getTodosProjetos :: String -> [Projeto]
getTodosProjetos filePath = do
    let arquivo = unsafePerformIO(B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Projeto]
    case decodedFile of
        Nothing -> []
        Just out -> out

-- Acho que faz mais sentido estar aqui, do que em Atividades
-- Adiciona uma atividade a um projeto
adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
adicionaAtividade atividade atividades = 
    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
        Just _-> atividades
        Nothing -> atividade : atividades

-- | Função que retorna a atividade de um projeto
getAtividadeDoProjeto :: Int -> [Atividade] -> Maybe Atividade
getAtividadeDoProjeto _ [] = Nothing
getAtividadeDoProjeto id (x:xs) 
  | id == (idAtividade x) = Just x
  | otherwise = getAtividadeDoProjeto id xs

-- | Função que remove projeto pelo ID
removeProjetoPorID :: Int -> [Projeto] -> [Projeto]
removeProjetoPorID _ [] = []
removeProjetoPorID idProjetoS(x:xs)
 | (idProjeto x) == idProjetoS = xs
 | otherwise = [x] ++ (removeProjetoPorID idProjetoS xs)

-- | Função que remove projeto da lista de usuarios reescrevendo o arquivo.
removerProjeto :: String -> Int -> IO()
removerProjeto jsonFilePath idProjeto = do
  let projetos = getTodosProjetos jsonFilePath
  let novosProjetos = removeProjetoPorID idProjeto projetos


  B.writeFile "../Temp.json" $ encode novosProjetos
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath


-- Obtem os IDs das atividades cadastradas em um projetos
-- getIdsAtividades :: Projeto -> [Atividade]
-- getIdsAtividades projeto = 
--     case atividades projeto of
--         Just atividadesProjeto -> [unlines atividadesProjeto] 
--         _ -> []

-- -- Obtem os IDs dos usuários cadastrados em um projetos
-- getIdsUsuarios :: Projeto -> [String]
-- getIdsUsuarios projeto = 
--     case membros projeto of
--         Just membrosProjeto -> [unlines membrosProjeto]
--         _ -> []



-- SERIA MELHOR EM PROJETO.hs?
-- Verifica se a Atividade já existe no sistema
-- verificaIdAtividade :: Projeto -> String -> Bool 
-- verificaIdAtividade projeto atividadeId = any (\idAtividade -> idAtividade == atividadeId) (getIdsAtividades projeto)
