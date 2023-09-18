{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Controllers.Atividades where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import System.Directory

instance FromJSON Atividade
instance ToJSON Atividade


data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    status :: String,
    idProjetoAtividade :: Int, 
    idAtividade :: Int,
    idMembroResponsavel :: Maybe Int,
    feedbacks :: [String]
} deriving (Show, Generic)


-- Cria uma atividade
criarAtividade :: String -> String -> String -> Int -> Int -> Maybe Int -> [String] -> IO()
criarAtividade   filePath titulo descricao idProjetoAtividade idAtividade idMembroResponsavel feedback = do
  let atividade = Atividade titulo descricao "Não atribuída!" idProjetoAtividade idAtividade idMembroResponsavel feedback
  escreverAtividade filePath atividade

-- Adiciona atividades à atividades.json
escreverAtividade :: String -> Atividade -> IO()
escreverAtividade filePath atividade = do

  let listaAtividades = (getTodasAtividades filePath) ++ [atividade]

  B.writeFile "../Temp.json" $ encode listaAtividades
  removeFile filePath
  renameFile "../Temp.json" filePath

-- Remove uma atividade da lista de atividades
apagarAtividade :: Int -> [Atividade] -> [Atividade]
apagarAtividade _ [] = []
apagarAtividade atividadeId (x:xs)
  | idAtividade x == atividadeId = xs
  | otherwise = x : apagarAtividade atividadeId xs

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

-- Verifica se um usuário (através do ID) é responsável por alguma atividade
ehMembroResponsavel :: Int -> [Atividade] -> Bool
ehMembroResponsavel membroResponsavelId atividades =
    any (\atividade ->
        case idMembroResponsavel atividade of
            Just responsavelId -> responsavelId == membroResponsavelId
            Nothing -> False
        ) atividades

-- Pega o status da atividade
getStatus :: Atividade -> String
getStatus atividade = status atividade

-- Adiciona um Feedback a uma atividade
-- adicionaFeedback :: Atividade -> String -> [String]
-- adicionaFeedback atividade novoFeedback = do
--     case (feedbacks atividade) of
--         Just feedbacksAtuais -> feedbacksAtuais ++ [novoFeedback]
--         Nothing -> [novoFeedback]

addFeedbackNaAtiv :: Int -> [Atividade] -> String -> [Atividade]
addFeedbackNaAtiv _ [] _ = []
addFeedbackNaAtiv id (ativ:ativs) novoFeedback
  | idAtividade ativ == id = ativ { feedbacks = feedbacks ativ ++ [novoFeedback] } : addFeedbackNaAtiv id ativs novoFeedback
  | otherwise = ativ : addFeedbackNaAtiv id ativs novoFeedback

-- | Função que edita o Json ao adicionar ou remover um projeto
editFeedbackDaAtividade :: String ->  Int ->  String -> IO()
editFeedbackDaAtividade jsonFilePath idAtividade feedback = do
  let listaAtividades = getTodasAtividades jsonFilePath
  let atividadesAtualizadas = addFeedbackNaAtiv idAtividade listaAtividades feedback

  B.writeFile "../Temp.json" $ encode atividadesAtualizadas
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

                     
-- Obtém os feedbacks da atividade
getFeedbacks :: Atividade -> [String]
getFeedbacks atividade = (feedbacks atividade)
    
-- -- | Cria um feedback
-- criarFeedbacks :: String -> Int -> String -> IO()
-- criarFeedbacks filePath idAtividade novoFeedback = do
--     let todasAtividades = (getTodasAtividades filePath)
--     let atividade = (getAtividade idAtividade todasAtividades)
--     case atividade of
--         Just atividadeEncontrada -> do
--                 let feedbacksAtualizados = (adicionaFeedback atividadeEncontrada novoFeedback)
--                 (deletarAtividade filePath idAtividade)
--                 let atividadesTemporarias = (getTodasAtividades filePath)
--                 let atividadesAtualizadas = atividadesTemporarias ++ [atividadeEncontrada]
                
--                 B.writeFile "../Temp.json" $ encode atividadesAtualizadas
--                 removeFile filePath
--                 renameFile "../Temp.json" filePath

--                 -- PRECISA RETIRAR OU MODIFICAR ESSE RETORNO
--                 mapM_ putStrLn $ feedbacksAtualizados
                
--         Nothing -> error "Atividade inexistente!"
    
-- | Obtém uma atividade a partir do ID
getAtividade :: Int -> [Atividade] -> Maybe Atividade
getAtividade _ [] = Nothing
getAtividade atividadeId (x:xs)
  | idAtividade x == atividadeId = Just x
  | otherwise = getAtividade atividadeId xs

-- | Obtém as todas atividades cadastradas no sistema
getTodasAtividades :: String -> [Atividade]
getTodasAtividades filePath = do
    let arquivo = unsafePerformIO(B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Atividade]
    case decodedFile of
        Nothing -> []
        Just out -> out

-- | Função que imprime as atividades para visualização
imprimirAtividade :: Atividade -> IO()
imprimirAtividade atividade = putStrLn $ "             Título: " ++ (titulo atividade) ++ "\n" ++
                                         "             Descrição: " ++ (descricao atividade) ++ "\n" ++
                                         "             ID Projeto: " ++ show (idProjetoAtividade atividade) ++ "\n" ++
                                         "             ID Atividade: " ++ show (idAtividade atividade) ++ "\n" ++
                                         "             Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
                                         "             Status: " ++ status atividade ++ "\n"




