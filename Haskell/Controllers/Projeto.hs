module Controllers.Projeto where

import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Controllers.Atividades as Atividade
import Database.Database


-- Definindo o tipo de dado Projeto
data Projeto = Projeto {
    idProjeto :: String,
    nomeProjeto :: String,
    descricaoProjeto :: String,
    idGerente :: String,
    membros :: Maybe [String],
    atividades :: Maybe [String]
} 


criaProjeto :: String -> String -> String -> String -> Maybe [String] -> Maybe [String] -> IO()
criaProjeto = addProjetoDatabase

-- Adiciona um projeto no sistema
adicionaProjeto :: Projeto -> [Projeto] -> [Projeto]
adicionaProjeto projeto projetos = 
    case find (\u -> nomeProjeto u == nomeProjeto projeto) projetos of 
        Just _-> projetos
        Nothing -> projeto : projetos


escreverProjeto :: FilePath -> [Projeto] -> IO ()
escreverProjeto arquivo projetos = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarProjeto projetos
    formatarProjeto projeto = "ID: " ++ idProjeto projeto ++ 
                            ", NOME: " ++ nomeProjeto projeto ++ 
                            ", DESCRICAO: " ++ descricaoProjeto projeto ++ 
                            ", IDGERENTE: " ++ idGerente projeto ++ 
                            ", ID S DE USUARIOS QUE TRABALHAM NO PROJETO: " ++ show (membros projeto) ++ 
                            ", ID S DE ATIVIDADES ANEXADAS AO PROJETO: " ++ show (atividades projeto)

-- Acho que faz mais sentido estar aqui, do que em Atividades
-- Adiciona uma atividade a um projeto
adicionaAtividade :: Atividade -> [Atividade] -> [Atividade]
adicionaAtividade atividade atividades = 
    case find (\u -> idAtividade u == idAtividade atividade) atividades of 
        Just _-> atividades
        Nothing -> atividade : atividades


lerProjetos :: FilePath -> IO [Projeto]
lerProjetos path = do
    conteudo <- readFile path
    let projetos = mapMaybe fromString $ lines conteudo
    return projetos

-- Obtem os IDs das atividades cadastradas em um projetos
getIdsAtividades :: Projeto -> [String]
getIdsAtividades projeto = 
    case atividades projeto of
        Just atividadesProjeto -> [unlines atividadesProjeto] 
        _ -> []

-- Obtem os IDs dos usuários cadastrados em um projetos
getIdsUsuarios :: Projeto -> [String]
getIdsUsuarios projeto = 
    case membros projeto of
        Just membrosProjeto -> [unlines membrosProjeto]
        _ -> []


fromString :: String -> Maybe Projeto
fromString str = case words str of
    [idProjeto, nomeProjeto, descricaoProjeto, idGerente, membrosStr, atividadesStr] -> do
        let atividades = words atividadesStr
        let membros = words membrosStr
        return Projeto { idProjeto = idProjeto,
                        nomeProjeto = nomeProjeto,
                        descricaoProjeto = descricaoProjeto,
                        idGerente = idGerente,
                        membros = Just membros,
                        atividades = Just atividades }
    _ -> Nothing

-- Remove um projeto do sistema
removeProjeto :: String -> IO()
removeProjeto = removeProjetoDatabase
