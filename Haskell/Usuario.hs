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
    idUsuario:: Int, nome:: String, senha:: String} deriving (Show, Read, Eq)


-- função que cadastra um usuário
cadastrarUsario :: Int -> String -> String -> Usuario
cadastraUsuario idUsuario nome senha = 
    (Usuario {idUsuario = idUsuario, nome = nome, senha = senha})


-- criar uma função para checar se é gerente



-- função que adiciona o usuário no sistema checando se já não existe (será possivel fazer essa checagem por id?)
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario usuarios = 
    case find (\u -> idUsuario u == idUsuario usuario) usuarios of
        Just _ -> usuarios
        Nothing -> usuario : usuarios


-- tentativa de função que remove um usuário (incompleta!!)
removerUsuario :: Int -> [Usuario] -> [Usuario]
removerUsuario idUsuario usuario = usuario { usuario =
    filter (\usuario -> idUsuario usuario/= idUsuario) }  


-- função que escreve os dados do usuário no txt
escreverUsuario :: FilePath -> [Usuario] -> IO ()
escreverUsuario arquivo usuarios = appendFile arquivo conteudo
    where
        conteudo = unline $ map formatarUsuario usuarios
        formatarUsuario u = "ID: " ++ show (idUsuario u) ++ ", NOME: " ++ nome u ++ ", SENHA: " ++ senha u


-- função que le os dados do usuario do txt
lerUsarios :: FilePath -> IO [Usuario.Usuario]
lerUsuarios path = do
    conteudo <- readFile path
    let usuarios = mapMaybe Usuario.fromString $ lines conteudo
    return usuarios


-- cria representação de um usuario em string?
fromString :: String -> Maybe Usuario
fromString str = case words str of
    [idStr, nome, senha] -> do
        idUsuario <- readMaybe idStr
        return Usuario{idUsuario = idUsuario, nome = nome, senha = senha}
    _ -> Nothing
