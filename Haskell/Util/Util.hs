module Util.Util where

import Data.Map as Map (fromList, Map)
import Controllers.Usuario as Usuario
import Controllers.Projeto as Projeto
import Data.Maybe (mapMaybe)
import Data.Char (toUpper)

import Data.Time (parseTimeM, defaultTimeLocale)
import Data.List
import System.IO.Unsafe (unsafePerformIO)

import qualified Atividades
import qualified Usuario
import qualified Projeto

-- <<< USUÁRIOS >>>

-- Verifica se o Usuario já existe no sistema
verificaIdUsuario :: String -> [Usuario] -> Bool
verificaIdUsuario id usuarios = elem id (map Usuario.idUsuario usuarios)

-- Verifica se a senha pertence ao usuário
verificaSenhaUsuario :: String -> Usuario -> Bool
verificaSenhaUsuario senha usuario = Usuario.senha == senha

-- Retorna a representação do usuário em String
getUsuario:: String -> [Usuario] -> Maybe String
getUsuario id usuarios =
  case filter (\u -> idUsuario u == id) usuarios of 
      [usuarioEncontrado] -> Just (formataUsuario usuarioEncontrado) 
      _ -> Nothing

-- Formata para String dados de um usuário
formataUsuario:: Usuario -> String
formataUsuario usuario = 
   "ID: " ++ show (idUsuario usuario) ++ "\n" ++
   "Nome: " ++ nome usuario ++ "\n"

-- Verifica se o usuário é gerente de algum projeto do sistema 
ehGerente :: Int -> [Projeto] -> Bool
ehGerente id gerentes = any (\projeto -> id == idGerente projeto) gerentes


-- <<< PROJETOS >>>

-- Verifica se o Projeto já existe no sistema
verificaIdProjeto :: String -> [Projeto.Projeto] -> Bool
verificaId projetoId projetos = not $ any (\projeto -> Projeto.idProjeto usuario == projetoId) projetos

-- Verifica se o nome do Projeto já existe no sistema
verificaNomeProjeto :: String -> [Projeto.Projeto] -> Bool
verificaNomeProjeto nome nomesProjetos = not $ any (\projeto -> nomeProjeto projeto == nome) nomesProjetos

-- Dá Upper Case para salvamento de dados afim de evitar na 'verificaçãoNomeProjeto' possivel erro ou geração de duplicata.
toUpperCase :: String -> String
toUpperCase str = map toUpper str


-- <<< ATIVIDADES >>>

-- Verifica se a Atividade já existe no sistema
verificaIdAtividade :: String -> [Atividades] -> Bool
verificaIdAtividade id atividades = elem id (map Atividades.idAtividade atividades)






