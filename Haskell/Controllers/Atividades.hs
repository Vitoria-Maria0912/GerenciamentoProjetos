{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Controllers.Atividades where



import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory
import Data.List (find)

import Controllers.Usuario

instance FromJSON Atividade
instance ToJSON Atividade

data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    status :: String,
    idProjetoAtividade :: Int, 
    idAtividade :: Int,
    idMembroResponsavel :: Maybe Int, ----------------
    feedback :: Maybe [String]
} deriving (Show, Generic)

--Íris
-- SALVAR Atividade no Banco
-- Formatar para String para o usiário ver 



-- Escreve atividades no arquivo.JSON
criarAtividade :: String -> String -> String -> Int -> Int -> Maybe Int -> Maybe [String] -> IO()
criarAtividade   filePath titulo descricao idProjetoAtividade idAtividade idMembroResponsavel feedback = do
  let atividade = Atividade titulo descricao "Não atribuída!" idProjetoAtividade idAtividade idMembroResponsavel feedback
  let listaAtividades = (getTodasAtividades filePath) ++ [atividade]

  B.writeFile "../Temp.json" $ encode listaAtividades
  removeFile filePath
  renameFile "../Temp.json" filePath

-- Remove uma atividade da lista de atividades
apagarAtividade :: Int -> [Atividade] -> [Atividade]
apagarAtividade _ [] = []
apagarAtividade id (x:xs)
  | idAtividade x == id = xs
  | otherwise = x : apagarAtividade id xs

-- Remove uma atividade do arquivo.JSON
deletarAtividade :: String -> Int -> IO()
deletarAtividade filePath idAtividade = do
    let atividades = getTodasAtividades filePath
    let atividadesAtualizadas = apagarAtividade idAtividade atividades

    B.writeFile "../Temp.json" $ encode atividadesAtualizadas
    removeFile filePath
    renameFile "../Temp.json" filePath

-- Muda o status de uma atividade
mudaStatus :: Atividade -> String -> Atividade
mudaStatus atividade novoStatus = atividade {status = novoStatus}

-- Pega o ID do membro responsável pela atividade
getMembroResponsavel :: Atividade -> String
getMembroResponsavel atividade = do
    let membroResponsavel = idMembroResponsavel atividade
    case membroResponsavel of
        Just _ -> show (idMembroResponsavel atividade)
        _ -> "Não atribuído!"

-- Pega o status da atividade
getStatus :: Atividade -> String
getStatus atividade = status atividade

-- Adiciona um Feedback a uma atividade
adicionaFeedback :: Atividade -> String -> Maybe [String]
adicionaFeedback atividade comentario = do
    let maybeFeedback = feedback atividade
    case maybeFeedback of
        Just feedbacksAtuais -> Just (comentario : feedbacksAtuais)
        Nothing -> Just [comentario]

-- Obtém uma atividade a partir do ID
getAtividade :: Int -> [Atividade] -> Maybe Atividade
getAtividade _ [] = Nothing
getAtividade id (x:xs)
  | idAtividade x == id = Just x
  | otherwise = getAtividade id xs

-- Obtém as todas atividades cadastradas no sistema
getTodasAtividades :: String -> [Atividade]
getTodasAtividades filePath = do
    let arquivo = unsafePerformIO(B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Atividade]
    case decodedFile of
        Nothing -> []
        Just out -> out

-- Exibe uma Atividade, em formato de lista com todos os seus atributos
getAtividadesToString :: Int -> [Atividade] -> Maybe [String] 
getAtividadesToString id atividades =
    case filter (\u -> idAtividade u == id) atividades of
        [atividadeEncontrada] -> Just (formataAtividade atividadeEncontrada)
        _ -> Nothing

-- Formata a atividade em uma lista com todos os seus atributos
formataAtividade:: Atividade -> [String]
formataAtividade atividade = ["Titulo: " ++ (titulo atividade) ++ "\n" ++
                              "Descrição: " ++ (descricao atividade) ++ "\n" ++
                              "ID Projeto: " ++ show (idProjetoAtividade atividade) ++ "\n" ++
                              "ID Atividade: " ++ show (idAtividade atividade) ++ "\n" ++
                              "ID Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
                              "Status: " ++ status atividade ++ "\n"]


