module Controllers.Usuario where
import System.IO
import Database.Database 


-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: String,
    nome :: String,
    senha :: String
}

-- cria um usuario
criaUsuario :: String -> String -> String -> Usuario
criasuario idUsuario nome senha = 
  (Usuario {idUsuario = idUsuario, nome = nome, senha = senha})

-- checa se existe um usuario com esse id, e se nao existe, adiciona na lista de usuários
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario usuarios =
    case find (\u -> idUsuario u == idUsuario usuario) usuarios of
        Just _ -> usuarios
        Nothing -> usuario : usuarios
        
-- escreve informações sobre usuários em um arquivo
escreverUsuarios :: FilePath -> [Usuario] -> IO ()
escreverUsuarios arquivo usuarios = appendFile arquivo conteudo
  where
    conteudo = unlines $ map formatarUsuario usuarios
    formatarUsuario u = "ID: " ++ show (idUsuario u) ++ ", NOME: " ++ nome u ++ ", FUNÇÃO: " ++ funcao u
    
-- ler informações sobre usuários de um arquivo -> retorna uma lista de usuários
lerUsuarios :: FilePath -> IO [Usuario.Usuario]
lerUsuarios path = do
  conteudo <- readFile path
  let usuarios = mapMaybe Usuario.fromString $ lines conteudo
  return usuarios
  
-- converte uma string em um objeto do tipo Usuario
fromString :: String -> Maybe Usuario
fromString str = case words str of
  [idStr, nome, funcao] -> do
    idUsuario <- readMaybe idStr
    return Usuario{idUsuario = idUsuario, nome = nome, funcao = funcao}
  _ -> Nothing
