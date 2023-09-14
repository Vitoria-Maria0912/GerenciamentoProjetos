{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Usuario where
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory

-- Definindo o tipo de dado Usuário
data Usuario = Usuario { 
    idUsuario :: Int,
    nome :: String,
    senha :: String,
    mensagens :: [Mensagem]
} deriving (Show, Generic)


instance FromJSON Usuario
instance ToJSON Usuario


-- | Função que retorna os dados de um usuário de acordo com o seu ID
getUsario:: Int-> [Usuario] -> Maybe Usuario
getUsario _ [] = Nothing
getUsario id (x:xs)     
  | idUsuario x == id = Just x
  | otherwise = getUsario id xs

-- | Função que retorna a lista atual de usuários cadastrados no sistema lendo o arquivo.
getUsuarios :: String -> [Usuario]
getUsuarios path = do
 let file = unsafePerformIO( B.readFile path )
 let decodedFile = decode file :: Maybe [Usuario]
 case decodedFile of
  Nothing -> []
  Just usuarios -> usuarios

-- | Função que imprime o usuário omitindo informação sensível
imprimirUsuario :: Usuario -> IO()
imprimirUsuario u = putStrLn $ "ID: " ++ show (idUsuario u) ++ ", Nome: " ++ nome u

-- | Função que salva e escreve o usuario no arquivo diretamente
salvarUsuario :: String -> Int -> String -> String -> IO()
salvarUsuario jsonFilePath idUsuario nome senha = do
 let u = Usuario idUsuario nome senha
 let userList = (getUsuarios jsonFilePath) ++ [u]

 B.writeFile "../Temp.json" $ encode userList
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath

-- | Função que remove usuario pelo ID
removeUsarioPorID :: Int -> [Usuario] -> [Usuario]
removeUsarioPorID _ [] = []
removeUsarioPorID idUsuarioS(x:xs)
 | (idUsuario x) == idUsuarioS = xs
 | otherwise = [x] ++ (removeUsarioPorID idUsuarioS xs)

-- | Função que remove usuario da lista de usuarios reescrevendo o arquivo.
removerUsuario :: String -> Int -> IO()
removerUsuario jsonFilePath idUsuario = do
 let usuarios = getUsuarios jsonFilePath
 let novosUsuarios = removeUsarioPorID idUsuario usuarios

 B.writeFile "../Temp.json" $ encode novosUsuarios
 removeFile jsonFilePath
 renameFile "../Temp.json" jsonFilePath


------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------PARTE USUÁRIO - MENSAGEM----------------------------------------------------------------------------------------------------------
-- função que envia mensagem geral para todos os usuários do projeto do REMETENTE (pegar no main o idUsuario e seu projeto referente)
-- *falta decidir se passa o caminho do filepath no main ou aqui.
enviarMensagemGeral :: Usuario -> String -> [Usuario] -> String -> IO ()
enviarMensagemGeral remetente conteudo usuarios jsonFilePath = do
    let updateUsuarios = map (\u -> if u == remetente then u { mensagens = MensagemGeral remetente conteudo : mensagens u } else u) usuarios
  -- B.writeFile jsonFilePath $ encode updatedUsuarios
    B.writeFile "../Temp.json" $ encode updateUsuarios
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath

-- função que envia mensagem privada para um usuário do projeto do Remetente (pegar no main o idusuario e seu projeto referente)
enviarMensagemPrivada :: Usuario -> Usuario -> String -> [Usuario] -> String -> IO ()
enviarMensagemPrivada remetente destinatario conteudo usuarios jsonFilePath = do
    let updateUsuarios = map (\u -> if u == remetente then u { mensagens = MensagemPrivada remetente destinatario conteudo : mensagens u } else u) usuarios
     B.writeFile "../Temp.json" $ encode updateUsuarios
    removeFile jsonFilePath
    renameFile "../Temp.json" jsonFilePath

-- função que pega todas as mensagens do usuário (caixa de mensagem)
getMensagensDoUsuario :: Usuario -> IO ()
mostrarMensagensDoUsuario usuario = do
    putStrLn "Mensagens do usuário:"
    mapM_ mostrarMensagem (mensagens usuario)

--formatação para exibição para o usuário
mostrarMensagem :: Mensagem -> IO ()
mostrarMensagem mensagem = do
    putStrLn $ "Remetente: " ++ nome (remetente mensagem)
    putStrLn $ "Conteúdo: " ++ conteudo mensagem
    putStrLn "-------------------"
----------------------------------------------------------------------------------------------------------------------------------------------------



main :: IO()
main = do
    --salvarUsuario "./dados.json" 1000 "Iris" "Iago"
    --salvarUsuario "./dados.json" 1750 "Yalle" "tuts"
    --putStrLn (show (getUsuario "./dados.json"))
   -- removerUsuario "./dados.json" 1
  let usuarios = getUsuarios "./dados.json"
  mapM_ imprimirUsuario usuarios 
    -- putStrLn (show (getUsarioPorID 2 (getUsuario "./dados.json")))
   -- salvarUsuario "./dados.json" 10 "Vitoria" "ruiva"
   -- putStrLn (show (getUsuario "./dados.json"))
   -- putStrLn (show (getNumDeUsuarios "./dados.json"))
