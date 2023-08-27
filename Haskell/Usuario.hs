module Usuario where
import Data.Char ()
import Data.Set ()
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory ()
import System.IO ()
import Data.List (find)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)


data Usuario = Usuario {
    idUsuario:: Int,
    nome:: String,
    senha:: String} deriving (Show, Read, Eq)


-- função que cadastra um usuário
cadastraUsuario :: Int -> String -> String -> Usuario
cadastraUsuario idUsuario nome senha =
    (Usuario {idUsuario = idUsuario, nome = nome, senha = senha})


-- Função para verificar se uma senha de usuário está correta
verificarSenha :: Usuario -> String -> String -> Bool
verificarSenha usuario senha senhaUsuario = senhaUsuario == senha


-- função que adiciona o usuário no sistema checando se já não existe (será possivel fazer essa checagem por id?)
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario usuarios =
    case find (\u -> idUsuario u == idUsuario usuario) usuarios of
        Just _ -> usuarios
        Nothing -> usuario : usuarios


-- Função para remover um usuário de uma lista de usuários e atualizar o arquivo .txt
removerUsuario :: Int -> [Usuario] -> FilePath -> IO [Usuario]
removerUsuario idUsuario usuarios arquivo = do
    let novosUsuarios = filter (\usuario -> idUsuario /= idUsuario) usuarios
    escreverUsuario arquivo novosUsuarios
    return novosUsuarios


-- função que escreve os dados do usuário no txt
escreverUsuario :: FilePath -> [Usuario] -> IO ()
escreverUsuario arquivo usuarios = appendFile arquivo conteudo
    where
        conteudo = unlines $ map formatarUsuario usuarios
        formatarUsuario u = "ID: " ++ show (idUsuario u) ++
         ", NOME: " ++ nome u ++ ", SENHA: " ++ senha u


-- função que le os dados do usuario do txt
lerUsuarios :: FilePath -> IO [Usuario.Usuario]
lerUsuarios path = do
    conteudo <- readFile path
    let usuarios = mapMaybe Usuario.fromString $ lines conteudo
    return usuarios


-- cria representação de um usuario em string
fromString :: String -> Maybe Usuario
fromString str = case words str of
    [idStr, nome, senha] -> do
        idUsuario <- readMaybe idStr
        return Usuario{idUsuario = idUsuario, nome = nome, senha = senha}
    _ -> Nothing
