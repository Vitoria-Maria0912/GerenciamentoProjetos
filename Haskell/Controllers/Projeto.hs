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
import qualified Data.ByteString.Lazy as B

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
--MENU GERAL:
-- -> cadastrarProjeto
-- -> visualizarProjetosPendentes

--MENU GERENTE:
-- -> em criaAtividade adicionar ao projeto
-- -> em deletaAtividade remover do projeto
-- -> removerProjeto
-- -> bancoDeAtividades

-- Definindo o tipo de dado Projeto
data Projeto = Projeto {
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: Int,
    membros :: Maybe [Usuario],
    atividades :: Maybe [Atividade]
} deriving (Show, Generic) 


criaProjeto :: Int -> String -> String -> Int  -> IO ()
criaProjeto jsonFilePath idProjeto nomeProjeto descricaoProjeto idGerente
    let projetos = lerProjetos jsonFilePath
    let projeto = Projeto idProjeto nomeProjeto descricaoProjeto idGerente 
    let projetosAtualizados = projetos ++ [projeto]

    B.writeFile "../Temp.json" $ encode projetosAtualizados
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath


-- Remove um projeto da lista de projetos
apagarProjeto :: Int -> [Projeto] -> [Projeto]
apagarProjeto _ [] = []
apagarProjeto id (x:xs)
  | idProjeto x == id = xs
  | otherwise = x : apagarProjeto id xs

-- Remove um projeto do arquivo JSON
--Passar a função 
deletarProjeto :: String -> Int -> IO ()
deletarProjeto filePath idProjeto = do
    let projetos = lerProjetos filePath
    let projetosAtualizados = apagarProjeto idProjeto projetos

    B.writeFile "../Temp.json" $ encode projetosAtualizados
    removeFile filePath
    renameFile "../Temp.json" filePath


-- Remove um projeto do sistema por meio do ID
removeProjetoPorID :: Int -> [Projeto] -> [Projeto]
removeProjetoPorID _ [] = [] -- Casamento de padrões para lista vazia
removeProjetoPorID idProjetoS (x:xs)
  | idProjeto x == idProjetoS = xs
  | otherwise = x : removeProjetoPorID idProjetoS xs



-- Verifica se o usuário é gerente de algum projeto do sistema 
ehGerente :: Int -> [Projeto] -> Bool
ehGerente id gerentes = any (\projeto -> id == idGerente projeto) gerentes

-- Obtém um projeto a partir do ID
getProjeto :: Int -> [Projeto] -> Maybe Projeto
getProjeto _ [] = Nothing
getProjeto id (x:xs)
  | idProjeto x == id = Just x
  | otherwise = getProjeto id xs

-- Recebe um ID de projeto e uma lista de projetos e retorna True se encontrar um projeto com o ID correspondente na lista
verificaIdProjeto :: Int -> [Projeto] -> Bool
verificaIdProjeto _ [] = False
verificaIdProjeto id (x:xs)
  | idProjeto x == id = True
  | otherwise = verificaIdProjeto id xs




-- SE DER PRECISA SER IMPLEMENTADO: muda status
--PRECISA SER IMPLEMENTADO: remove projeto do arquivo.JSON 
--PRECISA SER IMPLEMENTADO: adicionar o id de atividade (getAtividade)  a uma lista de atividade que tá em projeto . Get e retornar uma lista de atividades ou um IO e sobrescrever?
--Cria um novo projeto , sobrescreve  e apaga o antigo


-- editaAtividadesProjeto :: String -> Projeto-> [Int] -> Int -> IO()
-- editaAtividadesProjeto jsonFilePath projeto atividades idAtividade = do
--  let atividadesList = getTodasAtividades jsonFilePath 
--  let p = atividades atividades idAtividade
--  let newatividadesList = (deletarAtividade atividades atividadesList) ++ [p]

--  B.writeFile "../Temp.json" $ encode newatividadesList
--  removeFile jsonFilePath
--  renameFile "../Temp.json" jsonFilePath

-- removeatividadesJSON :: String -> Projeto-> Int -> IO()
-- removeatividadesJSON jsonFilePath projeto atividades = do
--  let atividadesList = getTodasAtividades jsonFilePath
--  let newatividadesList = deletarAtividade atividades atividadesList

--  B.writeFile "../Temp.json" $ encode newatividadesList
--  removeFile jsonFilePath
--  renameFile "../Temp.json" jsonFilePath



-- Acho que faz mais sentido estar aqui, do que em Atividades(Qual atividade ficaria em quaal projeto?)
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


-- -- Obtem os IDs das atividades cadastradas em um projetos(retorna todas as atividades em projeto)
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


--REFATORAR PARA MOSTRAR UM PROJETO ESPECIFICO
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




