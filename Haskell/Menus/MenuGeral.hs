module Menus.MenuGeral where

import qualified Data.Char as Char
import System.Exit (exitSuccess)
import System.Random
import Data.Char (toLower)
import Data.Maybe
import Controllers.Usuario
import Controllers.Projeto
import Menus.MenuGerente (menuRestritoProjeto)
import Menus.MenuPublico (menuPublicoProjeto, clearScreen, sairDoSistema)


-- | Exibe erro e retorna ao menu
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuPrincipal

-- | Menu principal com as principais funcionalidades
menuPrincipal :: IO ()
menuPrincipal = do

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "|                     Menu Principal                       |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|                  Selecione uma opção:                    |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|               C - Cadastrar novo usuário                 |" ++ "\n"
          ++ "|               D - Deletar perfil                         |" ++ "\n"
          ++ "|               P - Criar projeto                          |" ++ "\n"
          ++ "|               G - Menu de projetos                       |" ++ "\n"
          ++ "|               M - Caixa de mensagens                     |" ++ "\n"
          ++ "|               S - Sair do sistema                        |" ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  option <- getLine
  let lowerOption = map toLower option
  case lowerOption of
      "c" -> cadastrarUsuario
      "d" -> deletarUsuario
      "p" -> cadastrarProjeto
      "g" -> menuProjetos
    --   "m" -> chat
      "s" -> sairDoSistema
      _   -> erroMenuPrincipal

-- | Função que cadastra um usuário no sistema
cadastrarUsuario :: IO ()
cadastrarUsuario = do

    let userFilePath = "Database/usuarios.json"

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                        Cadastro:                           " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite seu nome: "

    nome <- getLine

    putStrLn "Digite sua senha: "
    senha <- getLine

    idUsuario <- randomRIO (0000, 9999 :: Int)

    let usuarioNoSistema = getUsuario idUsuario (getUsuarios userFilePath)

    case usuarioNoSistema of
      Just _ -> do
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|             Falha no cadastro! Tente novamente.          |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        cadastrarUsuario

      Nothing -> do
        salvarUsuario userFilePath idUsuario nome senha []
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "        Usuário cadastrado com sucesso! Seu ID é " ++ show idUsuario ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        menuPrincipal

-- | Deleta um usuário do sistema
deletarUsuario :: IO()
deletarUsuario = do

  let userFilePath = "Database/usuarios.json"

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "                      Deletar perfil:                       " ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  putStrLn "Digite seu ID:"
  idUsuario <- readLn :: IO Int

  let usuarioNoSistema = getUsuario idUsuario (getUsuarios userFilePath)

  case usuarioNoSistema of
    Just usuario -> do
      putStrLn "Digite sua senha: "
      senha <- getLine
      if verificaSenhaUsuario usuario senha then do
          removerUsuario userFilePath idUsuario
          clearScreen
          putStrLn $ ".----------------------------------------------------------." ++ "\n"
                  ++ "|                Perfil removido com sucesso!              |" ++ "\n"
                  ++ ".----------------------------------------------------------." ++ "\n"
          sairDoSistema
      else do
          clearScreen
          putStrLn $ ".------------------------------------------------------------." ++ "\n"
                  ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                  ++ ".------------------------------------------------------------." ++ "\n"
          deletarUsuario

    Nothing -> do
          clearScreen
          putStrLn $ ".-----------------------------------------------------------." ++ "\n"
                  ++ "|              ID inexistente! Tente novamente!             |" ++ "\n"
                  ++ ".-----------------------------------------------------------." ++ "\n"
          deletarUsuario

-- | Função que cadastra um projeto no sistema
cadastrarProjeto :: IO ()
cadastrarProjeto = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                      Criar projeto:                        " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite seu ID: "

    idUsuario <- readLn :: IO Int

    let usuarioNoSistema = getUsuario idUsuario (getUsuarios "Database/usuarios.json")

    if isJust(usuarioNoSistema) then do

        putStrLn "Digite o título do seu projeto: "
        nomeProjeto <- getLine

        putStrLn "Digite a descrição do projeto: "
        descricaoProjeto <- getLine

        idProjeto <- randomRIO (000, 999 :: Int)

        let projectFilePath = "Database/projetos.json"
        let projetoNoSistema = getProjeto idProjeto (getTodosProjetos projectFilePath)

        if isJust(projetoNoSistema) then do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|             Falha no cadastro! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                cadastrarProjeto
        else do
                criaProjeto projectFilePath idProjeto nomeProjeto descricaoProjeto idUsuario [] []
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "  Projeto criado com sucesso! O ID do seu projeto é: " ++ show idProjeto ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                menuPrincipal

        else do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                cadastrarProjeto

-- | Verifica se o usuário é gerente e mostra o menu correspondente
menuProjetos :: IO()
menuProjetos = do

    let projectFilePath = "Database/projetos.json"

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                    Menu de projetos:                       " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite seu ID:"
    idUsuario <- readLn :: IO Int

    let usuarios = getUsuarios "Database/usuarios.json"

    if isNothing (getUsuario idUsuario usuarios) then do
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|                ID inválido. Tente novamente!             |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        menuProjetos

    else do
        clearScreen
        let projetos = getTodosProjetos projectFilePath
        let gerente = ehGerente idUsuario projetos

        if gerente then menuRestritoProjeto
        else menuPublicoProjeto

-- Entra no chat
-- chat :: IO ()
-- chat = do
--   clearScreen
--   putStrLn $ ".----------------------------------------------------------." ++ "\n"
--           ++ "                     Caixa de Mensagens:                    " ++ "\n"
--           ++ ".----------------------------------------------------------." ++ "\n"
