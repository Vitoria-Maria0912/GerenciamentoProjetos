module Controllers.Usuario where
import System.IO
import Database.Database 
import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import Data.List (find)

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: String,
    nome :: String,
    senha :: String
}

-- Criação de um usuário
criaUsuario :: String -> String -> String -> Usuario
criaUsuario idUsuario nome senha = 
  (Usuario {idUsuario = idUsuario, nome = nome, senha = senha})

removerUsuario:: String -> [Usuario] -> [Usuario]
removerUsuario _ [] = []
removerUsuario idParaRemover (u:us)
  |idUsuario u == idParaRemover = us -- se o ID do usuário é igual ao ID a ser removido, omite esse usuario
  |otherwise = u : removerUsuario idParaRemover us  -- caso nao, mantem e chama recursivamente

-- Remoção de um Usuário, pelo ID
removeEAtualizaUsuarios :: String -> FilePath -> IO()
removeEAtualizaUsuarios idParaRemover arquivo = do 
    usuarios <- lerUsuarios arquivo
    let usuariosAtualizados = removerUsuario idParaRemover usuarios
    reescreverUsuarios arquivo usuariosAtualizados

reescreverUsuarios :: FilePath -> [Usuario] -> IO ()
reescreverUsuarios arquivo usuarios = withFile arquivo WriteMode $ \handle -> do
    let conteudo = unlines $ map formatarUsuario usuarios
    hPutStr handle conteudo
    hClose handle -- ver se precisa 


-- Checa se existe um usuario com esse id, e se nao existe, adiciona na lista de usuários
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario usuarios =
    case find (\u -> idUsuario u == idUsuario usuario) usuarios of
        Just _ -> usuarios
        Nothing -> usuario : usuarios

-- escreve informações sobre usuários em um arquivo (funcionando)
escreverUsuarios :: FilePath -> [Usuario] -> IO ()
escreverUsuarios arquivo usuarios = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarUsuario usuarios
  


-- ler informações sobre usuários de um arquivo -> retorna uma lista de usuários
lerUsuarios :: FilePath -> IO [Usuario]
lerUsuarios path = do
  conteudo <- readFile path
  let usuarios = mapMaybe fromString $ lines conteudo
  return usuarios

-- converte uma string em um objeto do tipo Usuario
fromString :: String -> Maybe Usuario
fromString str = case words str of
  [idUsuario, nome, senha] -> do
    return Usuario{idUsuario = idUsuario, nome = nome, senha = senha}
  _ -> Nothing

formatarUsuario :: Usuario -> String
formatarUsuario u = "ID: " ++ show (idUsuario u) ++ 
                   ", NOME: " ++ nome u ++ 
                   ", SENHA: " ++ senha u
    
formatarUsuario :: Usuario -> String
formatarUsuario u = "ID: " ++ show (idUsuario u) ++ 
                   ", NOME: " ++ nome u ++ 
                   ", SENHA: " ++ senha u
