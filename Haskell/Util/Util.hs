module Util.Util where

import Data.Map as Map (fromList, Map)
import Controllers.Usuario as Usuario
import Controllers.Projeto as Projeto
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)

import Data.Time (parseTimeM, defaultTimeLocale)
import Data.List
import System.IO.Unsafe (unsafePerformIO)

import Controllers.Atividades
import Controllers.Usuario
import Controllers.Projeto

-- by Yalle
import Control.Monad (mapM)

-- <<< USUÁRIOS >>>

-- -- Verifica se o Usuario já existe no sistema
verificaIdUsuario :: String -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)

-- Verifica se a senha pertence ao usuário
verificaSenhaUsuario :: String -> Usuario -> Bool
verificaSenhaUsuario senhaUsuario usuario = (senha usuario == senhaUsuario)

-- Retorna a representação do usuário em String
getUsuarioToString:: String -> [Usuario] -> Maybe String
getUsuarioToString id usuarios =
  case filter (\u -> idUsuario u == id) usuarios of 
      [usuarioEncontrado] -> Just (formataUsuario usuarioEncontrado) 
      _ -> Nothing
  where 
        formataUsuario usuario = "ID: " ++ show (idUsuario usuario) ++ "\n" ++
                                 "Nome: " ++ nome usuario ++ "\n"

getUsuario :: String -> [Usuario] -> Usuario
getUsuario id usuarios = case filter (\u -> idUsuario u == id) usuarios of
                              [usuarioEncontrado] -> usuarioEncontrado
                              _ -> error "Usuário não encontrado!"

-- Verifica se o usuário é gerente de algum projeto do sistema 
ehGerente :: String -> [Projeto] -> Bool
ehGerente id gerentes = any (\projeto -> id == idGerente projeto) gerentes


-- -- <<< PROJETOS >>>

-- Verifica se o Projeto já existe no sistema
-- verificaIdProjeto :: String -> [Projeto.Projeto] -> Bool
-- verificaId projetoId projetos = not $ any (\projeto -> Projeto.idProjeto usuario == projetoId) projetos

-- Verifica se o nome do Projeto já existe no sistema
verificaNomeProjeto :: String -> [Projeto.Projeto] -> Bool
verificaNomeProjeto nome nomesProjetos = not $ any (\projeto -> nomeProjeto projeto == nome) nomesProjetos

getProjeto :: String -> [Projeto] -> Projeto
getProjeto id projetos = case filter (\u -> idProjeto u == id) projetos of
                              [projetoEncontrado] -> projetoEncontrado
                              _ -> error "Projeto não encontrado!"

-- Dá Upper Case para salvamento de dados afim de evitar na 'verificaçãoNomeProjeto' possivel erro ou geração de duplicata.
toUpperCase :: String -> String
toUpperCase str = map toUpper str


-- <<< ATIVIDADES >>>

-- Verifica se a Atividade já existe no sistema
verificaIdAtividade :: Projeto -> String -> Bool 
verificaIdAtividade projeto atividadeId = any (\idAtividade -> idAtividade == atividadeId) (getIdsAtividades projeto)

-- Exibe uma Atividade, em formato de lista com todos os seus atributos
getAtividadesToString :: String -> [Atividade] -> Maybe [String] 
getAtividadesToString id atividades =
    case filter (\u -> idAtividade u == id) atividades of
        [atividadeEncontrada] -> Just (formataAtividade atividadeEncontrada)
        _ -> Nothing

getAtividade :: String -> [Atividade] -> Atividade
getAtividade id atividades = case filter (\u -> idAtividade u == id) atividades of
                                  [atividadeEncontrada] -> atividadeEncontrada
                                  _ -> error "Atividade não encontrada!"

-- Formata a atividade em uma lista com todos os seus atributos
formataAtividade:: Atividade -> [String]
formataAtividade atividade = ["Titulo: " ++ (titulo atividade) ++ "\n" ++
                            "Descrição: " ++ (descricao atividade) ++ "\n" ++
                            "ID Projeto: " ++ (idProjetoAtividade atividade) ++ "\n" ++
                            "ID Atividade: " ++ (idAtividade atividade) ++ "\n" ++
                            "ID Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
                            "Status: " ++ status atividade ++ "\n"]

-- by Yalle
-- Escreve atividades no arquivo.txt
escreverAtividades :: FilePath -> [Atividade] -> IO ()
escreverAtividades arquivo atividades = appendFile arquivo conteudo
  where
    conteudo = unlines (concatMap formataAtividade atividades)
