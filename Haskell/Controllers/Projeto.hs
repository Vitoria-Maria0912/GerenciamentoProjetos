{-# LANGUAGE DeriveGeneric #-} 
module Controllers.Projeto where

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


-- Definindo o tipo de dado Projeto
data Projeto = Projeto {
    idProjeto :: Int,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: Int,
    membros :: [Int],
    atividades :: [Int]
} deriving (Show, Generic) 


-- Cria um novo projeto e o adiciona ao arquivo JSON
criaProjeto :: String -> Int -> String -> String -> Int -> [Int] -> [Int] -> IO ()
criaProjeto jsonFilePath idProjeto nomeProjeto descricaoProjeto idGerente membros atividades = do
    let projeto = Projeto idProjeto nomeProjeto descricaoProjeto idGerente membros atividades
    let projetosAtualizados = (getTodosProjetos jsonFilePath) ++ [projeto]

    B.writeFile "../Temp.json" $ encode projetosAtualizados
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath

-- -- MUDEI AQUI ------------------------------------------------- INCOMPLETO
-- deletarAtividadeProjeto :: String -> Int -> Int -> IO()
-- deletarAtividadeProjeto filePath idProjeto idAtividade = do
--   deletarAtividade filePath idAtividade

-- -- MUDEI AQUI ------------------------------------------------- INCOMPLETO
-- adicionaAtividadeAoProjeto :: String -> String -> String -> Int -> Int -> Maybe Int -> Maybe [String] -> IO()
-- adicionaAtividadeAoProjeto filePath titulo descricao idProjetoAtividade idAtividade idMembroResponsavel feedback = do
--   criarAtividade "Database/atividades.json" titulo descricao idProjetoAtividade idAtividade idMembroResponsavel Nothing

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

-- | Função que adiciona uma atividade de um projeto
addAtivNoProjeto :: Int -> [Projeto] -> Int -> [Projeto]
addAtivNoProjeto _ [] _ = []
addAtivNoProjeto id (proj:projs) novaAtiv
  | idProjeto proj == id = proj { atividades = atividades proj ++ [novaAtiv] } : addAtivNoProjeto id projs novaAtiv
  | otherwise = proj : addAtivNoProjeto id projs novaAtiv

-- | Função que remove uma atividade de um projeto
removeAtivDoProjeto :: Int -> [Projeto] -> Int -> [Projeto]
removeAtivDoProjeto _ [] _ = []
removeAtivDoProjeto id (proj:projs) ativARemover
  | idProjeto proj == id = proj { atividades = filter (/= ativARemover) (atividades proj) } : removeAtivDoProjeto id projs ativARemover
  | otherwise = proj : removeAtivDoProjeto id projs ativARemover

-- | Função que edita o Json ao adicionar ou remover um projeto
editAtivDoProjeto :: String ->  Int ->  Int -> Bool -> IO()
editAtivDoProjeto jsonFilePath idProjeto idAtividade adicionar = do
  let listaProjetos = getTodosProjetos jsonFilePath
  let projetosAtualizados = if adicionar
                            then addAtivNoProjeto idProjeto listaProjetos idAtividade
                            else removeAtivDoProjeto idProjeto listaProjetos idAtividade

  B.writeFile "../Temp.json" $ encode projetosAtualizados
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

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

-- | Função que adiciona membro em um projeto
addMembroNoProjeto :: Int -> [Projeto] -> Int -> [Projeto]
addMembroNoProjeto _ [] _ = []
addMembroNoProjeto id (proj:projs) novoMembro
  | idProjeto proj == id = proj { membros = membros proj ++ [novoMembro] } : addMembroNoProjeto id projs novoMembro
  | otherwise = proj : addMembroNoProjeto id projs novoMembro

-- | Função que edita os membros do projeto no JSON (adição ou remoção de membros)
editMembrosDoProjeto :: String -> Int -> Int -> Bool -> IO ()
editMembrosDoProjeto jsonFilePath idProjeto idMembro adicionar = do
  let listaProjetos = getTodosProjetos jsonFilePath
  let projetosAtualizados = if adicionar
                            then addMembroNoProjeto idProjeto listaProjetos idMembro
                            else removeMembroDoProjeto idProjeto listaProjetos idMembro

  B.writeFile "../Temp.json" $ encode projetosAtualizados
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

-- | Função que remove membro em um projeto
removeMembroDoProjeto :: Int -> [Projeto] -> Int -> [Projeto]
removeMembroDoProjeto _ [] _ = []
removeMembroDoProjeto id (proj:projs) membroARemover
  | idProjeto proj == id = proj { membros = filter (/= membroARemover) (membros proj) } : removeMembroDoProjeto id projs membroARemover
  | otherwise = proj : removeMembroDoProjeto id projs membroARemover

-- | Função que obtem os IDs dos usuários cadastrados em um projetos
getIdsMembrosDoProjeto :: Projeto -> [Int]
getIdsMembrosDoProjeto projeto = (membros projeto)

-- | Função que imprime a lista de ids (POSSIVELMENTE PODE IR PRA UTIL?)
-- imprimirListaDeIds :: [Int] -> String
-- imprimirListaDeIds lista = unwords (map show lista)

-- | Função que checa se o membro está na lista de membros de um projeto
membroEstaNoProjeto :: Int -> Projeto -> Bool
membroEstaNoProjeto idMembro projeto = elem idMembro (getIdsMembrosDoProjeto projeto)

-- | Função que imprime os projetos para visualização
imprimirProjetos :: Projeto -> IO()
imprimirProjetos projeto = putStrLn $ "             Título: " ++ nomeProjeto projeto 
                                   ++ " (ID: " ++ show (idProjeto projeto) ++ ")"

-- | Função que verifica se uma atividade está no projeto
atividadeEstaNoProjeto :: Int -> Projeto -> [Atividade] -> Bool
atividadeEstaNoProjeto idAtividade projeto listaAtividades = 
  case getAtividadeDoProjeto idAtividade listaAtividades of
    Just _  -> idAtividade `elem` (atividades projeto)
    Nothing -> False

-- | Função que imprime as atividades do projeto
imprimeAtividadesDoProjeto :: Atividade -> IO()
imprimeAtividadesDoProjeto atividadeDoProjeto = imprimirAtividade atividadeDoProjeto

-- | Função que retorna as atividades correspondentes aos IDs de um projeto
getAtividadesDoProjeto :: [Int] -> [Atividade] -> [Atividade]
getAtividadesDoProjeto listaIds atividades =
  [atividadesDoProjeto | atividadesDoProjeto <- atividades, (idAtividade atividadesDoProjeto) `elem` listaIds]

-- | Função que imprime as atividades do projeto
imprimeMembrosDoProjeto :: Usuario -> IO()
imprimeMembrosDoProjeto membroDoProjeto = imprimirUsuario membroDoProjeto

-- | Função que retorna as atividades correspondentes aos IDs de um projeto
getMembrosDoProjeto :: [Int] -> [Usuario] -> [Usuario]
getMembrosDoProjeto listaIds usuarios =
  [membrosDoProjeto | membrosDoProjeto <- usuarios, (idUsuario membrosDoProjeto) `elem` listaIds]

---------------------------------CAIXA_DE_MENSAGEM E PROJETO-----------------------------------------------------------------------------------------------------------------------------
--Função que lista com base no ID, os projetos que está participando
listarProjetosDoUsuario :: Int -> [Projeto] -> String
listarProjetosDoUsuario id projetos =
    let projetosDoUsuario = filter (\projeto -> 
            usuarioEstaEmAlgumProjeto id [projeto] || ehGerente id [projeto]) projetos
    in
    "ID's dos Projetos que há participação:\n" ++ formatarIdsProjetos projetosDoUsuario

--formata a saída dos ID dos projetos que o Usuário participa
formatarIdsProjetos :: [Projeto] -> String
formatarIdsProjetos [] = "Nenhum projeto encontrado."
formatarIdsProjetos projetos =
    unlines (map (\projeto -> "■ " ++ show (idProjeto projeto) ++ " - " ++ (nomeProjeto projeto)) projetos)

-- Verifica se um usuário está presente em algum projeto do sistema
usuarioEstaEmAlgumProjeto :: Int -> [Projeto] -> Bool
usuarioEstaEmAlgumProjeto _ [] = False
usuarioEstaEmAlgumProjeto id (projeto:projetosRestantes) =
    if id `elem` membros projeto
        then True
        else usuarioEstaEmAlgumProjeto id projetosRestantes