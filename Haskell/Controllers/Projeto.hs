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

-- import Data.Char ()
-- import Data.Set ()
-- import qualified Data.Text.IO as TIO
-- import Data.Time
-- import System.Directory ()
-- import System.IO ()
-- import Data.List (find)
-- import Data.Maybe (mapMaybe)
-- import Text.Read (readMaybe)
import Controllers.Usuario
import Controllers.Atividades
-- import Database.Database

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

--Salva e cria projeto
salvarProjeto :: String -> Projeto -> IO ()
salvarProjeto jsonFilePath projeto = do
    let projetos = lerProjetos jsonFilePath
    let novoId = length projetos + 1
    let projetoComId = projeto { idProjeto = novoId }
    let projetosAtualizados = projetos ++ [projetoComId]

    B.writeFile "../Temp.json" $ encode projetosAtualizados
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath


-- Verifica se o usuário é gerente de algum projeto do sistema 
ehGerente :: Int -> [Projeto] -> Bool
ehGerente id gerentes = any (\projeto -> id == idGerente projeto) gerentes

-- Obtém um projeto a partir do ID
getProjeto :: Int -> [Projeto] -> Maybe Projeto
getProjeto _ [] = Nothing
getProjeto id (x:xs)
  | idProjeto x == id = Just x
  | otherwise = getProjeto id xs


-- Remove um projeto do sistema por meio do ID
removeProjetoPorID :: Int -> [Projeto] -> [Projeto]
removeProjetoPorID _ [] = [] -- Casamento de padrões para lista vazia
removeProjetoPorID idProjetoS (x:xs)
  | idProjeto x == idProjetoS = xs
  | otherwise = x : removeProjetoPorID idProjetoS xs



-- Adiciona um projeto no sistema
-- adicionaProjeto :: Projeto -> [Projeto] -> [Projeto]
-- adicionaProjeto projeto projetos = 
--     case find (\u -> nomeProjeto u == nomeProjeto projeto) projetos of 
--         Just _-> projetos
--         Nothing -> projeto : projetos



-- Acho que faz mais sentido estar aqui, do que em Atividades
-- Adiciona uma atividade a um projeto
adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
adicionaAtividade atividade atividades = 
    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
        Just _-> atividades
        Nothing -> atividade : atividades

-- Lê o arquivo projetos.json
lerProjetos :: String -> [Projeto]
lerProjetos filePath = do
    let arquivo = unsafePerformIO(B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Projeto]
    case decodedFile of
        Nothing -> []
        Just out -> out

getAtividadeDoProjeto :: Int -> [Atividade] -> Maybe Atividade
getAtividadeDoProjeto _ [] = Nothing
getAtividadeDoProjeto id (x:xs) 
  | id == (idAtividade x) = Just x
  | otherwise = getAtividadeDoProjeto id xs


-- -- Obtem os IDs das atividades cadastradas em um projetos
-- getIdsAtividades :: Projeto -> [String]
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


-- fromString :: String -> Maybe Projeto
-- fromString str = case words str of
--     [idProjeto, nomeProjeto, descricaoProjeto, idGerente, membrosStr, atividadesStr] -> do
--         let atividades = words atividadesStr
--         let membros = words membrosStr
--         return Projeto { idProjeto = idProjeto,
--                         nomeProjeto = nomeProjeto,
--                         descricaoProjeto = descricaoProjeto,
--                         idGerente = idGerente,
--                         membros = Just membros,
--                         atividades = Just atividades }
--     _ -> Nothing


-- -- Escreve um projeto no arquivo.txt
-- escreverProjeto :: FilePath -> [Projeto] -> IO ()
-- escreverProjeto arquivo projetos = appendFile arquivo conteudo
--   where
--     conteudo = unlines $ map formatarProjeto projetos
--     formatarProjeto projeto = "ID: " ++ show (idProjeto projeto) ++ 
--                             ", NOME: " ++ nomeProjeto projeto ++ 
--                             ", DESCRICAO: " ++ descricaoProjeto projeto ++ 
--                             ", IDGERENTE: " ++ show (idGerente projeto) ++ 
--                             ", ID S DE USUARIOS QUE TRABALHAM NO PROJETO: " ++ show (membros projeto) ++ 
--                             ", ID S DE ATIVIDADES ANEXADAS AO PROJETO: " ++ show (atividades projeto)


-- SERIA MELHOR EM PROJETO.hs?
-- Verifica se a Atividade já existe no sistema
-- verificaIdAtividade :: Projeto -> String -> Bool 
-- verificaIdAtividade projeto atividadeId = any (\idAtividade -> idAtividade == atividadeId) (getIdsAtividades projeto)



--PRECISA SER IMPLEMENTADO: adicionar uma atividade a um projeto
