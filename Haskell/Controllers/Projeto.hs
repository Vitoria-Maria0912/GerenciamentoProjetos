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
    usuarios :: [String],
    atividades :: [String]
} 


-- criação de projeto
criaProjeto :: String -> String -> String -> String -> [String] -> [String] -> Projeto
criaProjeto idProjeto nomeProjeto descricao idGerente usuarios atividades = 
    Projeto {idProjeto = idProjeto, 
            nomeProjeto = nomeProjeto, 
            descricaoProjeto = descricao, 
            idGerente = idGerente, 
            usuarios = usuarios,  
            atividades = atividades}

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
                            ", ID S DE USUARIOS QUE TRABALHAM NO PROJETO: " ++ show (usuarios projeto) ++ 
                            ", ID S DE ATIVIDADES ANEXADAS AO PROJETO: " ++ show (atividades projeto)


lerProjetos :: FilePath -> IO [Projeto]
lerProjetos path = do
    conteudo <- readFile path
    let projetos = mapMaybe fromString $ lines conteudo
    return projetos

-- Obtem os IDs das atividades cadastradas em um projetos
getIdsAtividades :: [Projeto] -> [String]
getIdsAtividades projetos = concatMap atividades projetos

-- Obtem os IDs dos usuários cadastrados em um projetos
getIdsUsuarios :: [Projeto] -> [String]
getIdsUsuarios projetos = concatMap usuarios projetos

fromString :: String -> Maybe Projeto
fromString str = case words str of
    [idProjeto, nomeProjeto, descricaoProjeto, idGerente,usuariosStr, atividadesStr] -> do
        let atividades = words atividadesStr
        let usuarios = words usuariosStr
        return Projeto { idProjeto = idProjeto,
                        nomeProjeto = nomeProjeto,
                        descricaoProjeto = descricaoProjeto,
                        idGerente = idGerente,
                        usuarios = usuarios,
                        atividades = atividades }
    _ -> Nothing

-- Remove um projeto do sistema (comentado pra excluir da database)
-- removeProjeto :: String -> IO()
-- removeProjeto = removeProjetoDatabase
