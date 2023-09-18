{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Controllers.Atividades where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.List (find)

instance FromJSON Atividade
instance ToJSON Atividade


data Atividade = Atividade {
    titulo :: String,
    descricao :: String,
    status :: String,
    idProjetoAtividade :: Int, 
    idAtividade :: Int,
    idMembroResponsavel :: Maybe Int, 
    feedbacks :: Maybe [String]
} deriving (Show, Generic)

-----------------------------------------------------BANCO DE ATIVIDADES

-- Define o caminho para o arquivo JSON do banco de atividades
bancoAtividadesJSON :: FilePath
bancoAtividadesJSON = "Database/bancoDeAtividades.json"

-- Função para ler o arquivo JSON e obter as atividades como uma lista de listas de atividades
lerBancoDeAtividades :: IO [[Atividade]]
lerBancoDeAtividades = do
    conteudo <- B.readFile bancoAtividadesJSON
    let atividades = fromMaybe [] (decode conteudo)
    return atividades

-- Função para adicionar uma nova atividade ao banco de atividades
adicionarAtividadeAoJSON :: Atividade -> IO ()
adicionarAtividadeAoJSON novaAtividade = do
    -- Lê o banco de atividades atual
    atividadesAtuais <- lerBancoDeAtividades
    -- Adiciona a nova atividade à lista de atividades atuais
    let atividadesAtualizadas = [novaAtividade] : atividadesAtuais
    -- Salva as atividades atualizadas de volta no arquivo JSON
    B.writeFile bancoAtividadesJSON (encode atividadesAtualizadas)

-- Função para consultar uma atividade por ID
consultarAtividadePorID :: Int -> IO (Maybe Atividade)
consultarAtividadePorID id = do
    atividades <- lerBancoDeAtividades
    return $ find (\atividade -> idAtividade atividade == id) (concat atividades)

-- Função para filtrar atividades por status
filtrarAtividadesPorStatus :: String -> IO [[Atividade]]
filtrarAtividadesPorStatus status = do
    atividades <- lerBancoDeAtividades
    return $ filter (\atividade -> status atividade == status) (concat atividades)

-- | Função que imprime os detalhes de uma única atividade
imprimirDetalhesAtividade :: Atividade -> IO ()
imprimirDetalhesAtividade atividade = putStrLn $ unlines
    [ "Detalhes da Atividade:"
    , "Título: " ++ titulo atividade
    , "Descrição: " ++ descricao atividade
    , "ID Projeto: " ++ show (idProjetoAtividade atividade)
    , "ID Atividade: " ++ show (idAtividade atividade)
    , "Membro Responsável: " ++ show (idMembroResponsavel atividade)
    , "Status: " ++ status atividade
    ]
 
------------------------------------Encerra tentativa de Banco de Atividades


-- Cria uma atividade
criarAtividade :: String -> String -> String -> Int -> Int -> Maybe Int -> Maybe [String] -> IO()
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

-- Lê o arquivo projetos.json
lerAtividades :: String -> [Atividade]
lerAtividades filePath = do
    let arquivo = unsafePerformIO(B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Atividade]
    case decodedFile of
        Nothing -> []
        Just out -> out

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
adicionaFeedback :: Atividade -> String -> [String]
adicionaFeedback atividade novoFeedback = do
    case (feedbacks atividade) of
        Just feedbacksAtuais -> feedbacksAtuais ++ [novoFeedback]
        Nothing -> [novoFeedback]

-- Obtém os feedbacks da atividade
getFeedbacks :: Atividade -> Maybe [String]
getFeedbacks atividade = (feedbacks atividade)
    
-- Remove uma atividade do arquivo.JSON
criarFeedbacks :: String -> Int -> String -> IO()
criarFeedbacks filePath idAtividade novoFeedback = do
    let todasAtividades = (getTodasAtividades filePath)
    let atividade = (getAtividade idAtividade todasAtividades)
    case atividade of
        Just atividadeEncontrada -> do
                let feedbacksAtualizados = (adicionaFeedback atividadeEncontrada novoFeedback)
                (deletarAtividade filePath idAtividade)
                let atividadesTemporarias = (getTodasAtividades filePath)
                let atividadesAtualizadas = atividadesTemporarias ++ [atividadeEncontrada]
                
                B.writeFile "../Temp.json" $ encode atividadesAtualizadas
                removeFile filePath
                renameFile "../Temp.json" filePath

                -- PRECISA RETIRAR OU MODIFICAR ESSE RETORNO
                mapM_ putStrLn $ feedbacksAtualizados
                
        Nothing -> error "Atividade inexistente!"
    

-- Obtém uma atividade a partir do ID
getAtividade :: Int -> [Atividade] -> Maybe Atividade
getAtividade _ [] = Nothing
getAtividade atividadeId (x:xs)
  | idAtividade x == atividadeId = Just x
  | otherwise = getAtividade atividadeId xs

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
getAtividadesToString atividadeId atividades =
    case filter (\u -> idAtividade u == atividadeId) atividades of
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


