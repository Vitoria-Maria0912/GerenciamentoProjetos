module Menus.MenuGeral where

import qualified Data.Char as Char
import System.Exit (exitSuccess)
import System.Random
import Data.Char (toLower)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Controllers.Usuario
import Controllers.Projeto
import Controllers.CaixadeMensagem
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
      "m" -> menuChat
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

-- -- Entra no chat
menuChat :: IO ()
menuChat = do
  clearScreen
  putStrLn $ ".-----------------------------------------------------------------------------." ++ "\n"
          ++ "|                   Bem-vindo ao Chat !                                       |" ++ "\n"
          ++ "|       Envie mensagens entre membros do seu projeto e usuários do sistema    |" ++ "\n"
          ++ "|                  Selecione uma opção:                                       |" ++ "\n"
          ++ "|                                                                             |" ++ "\n"
          ++ "|          C - Visualizar mensagens gerais de um projeto                      |" ++ "\n"
          ++ "|          H - Visualizar mensagens privadas                                  |" ++ "\n"
          ++ "|          A - Enviar mensagem geral para membros do projeto                  |" ++ "\n"
          ++ "|          T - Enviar mensagem privada                                        |" ++ "\n"
          ++ "|          S - Sair do sistema                                                |" ++ "\n"
          ++ ".-----------------------------------------------------------------------------." ++ "\n"

  option <- getLine
  let lowerOption = map toLower option
  case lowerOption of
      "c" -> visualizarMensagensGerais
      "h" -> visualizarMensagensPrivadas
      "a" -> enviarMGeral
      "t" -> enviarMPrivada
      "s" -> sairDoSistema
      _   -> erroMenuChat
      
-- | Exibe erro e retorna ao menuChat
erroMenuChat :: IO()
erroMenuChat = do
    
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    threadDelay 1000000
    menuChat

visualizarMensagensGerais :: IO()
visualizarMensagensGerais = do
        --Identificação do Usuário
        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int
      
        --Pega o usuario(idUsuario) e projetos cadastrados no sistema
        let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))
        let projetos = getTodosProjetos "Database/projetos.json"
        let mensagen = getMensagens "Database/mensagens.json"


        case (usuarioNoSistema, projetos) of
                (Just usuario, projetos) -> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                if (usuarioEstaEmAlgumProjeto idUsuario projetos || ehGerente idUsuario projetos)then do
                                        putStrLn $ ".------------------------------------------------------------------." ++ "\n"
                                                ++ "| Digite o id do Projeto que deseja visualizar as mensagens gerais |" ++ "\n"
                                                ++ ".------------------------------------------------------------------." ++ "\n"
                                      
                                        let projetos_user = listarProjetosDoUsuario idUsuario projetos
                                        putStrLn projetos_user
                                        idEscolhido <- readLn :: IO Int

                                        putStrLn "\nCarregando...\n  "
                                        threadDelay 1500000
                                        putStrLn "--------------------------------------------------------------------"
                                        exibeMensagens mensagen idEscolhido
                                        threadDelay 1500000
                                        menuRetorno

                                else do
                                        clearScreen
                                        putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                                ++ "|         Usuário não está cadastrado em nenhum projeto.     |" ++ "\n"
                                                 ++ ".-----------------------------------------------------------." ++ "\n"
                                        menuRetorno
                                
                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                         ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                         ++ ".------------------------------------------------------------." ++ "\n"
                                menuRetorno
                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        menuRetorno

visualizarMensagensPrivadas :: IO()
visualizarMensagensPrivadas = do
        --Identificação do Usuário
        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int

        let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))
        let projetos = getTodosProjetos "Database/projetos.json"


        case (usuarioNoSistema, projetos) of
                (Just usuario, projetos) -> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                if (usuarioEstaEmAlgumProjeto idUsuario projetos || ehGerente idUsuario projetos)then do
                                        putStrLn "Mensagens Privadas:"
                                        let projetos_user = listarProjetosDoUsuario idUsuario projetos
                                        let mensagen = getMensagens "Database/mensagens.json"
                                        putStrLn "Carregando...\n  "
                                        threadDelay 100000
                                        exibeMensagens mensagen idUsuario
                                        menuRetorno
                                 else do
                                        clearScreen
                                        putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                                ++ "|         Usuário não está cadastrado em nenhum projeto.     |" ++ "\n"
                                                 ++ ".-----------------------------------------------------------." ++ "\n"
                                        menuRetorno
                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                         ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                         ++ ".------------------------------------------------------------." ++ "\n"
                                menuRetorno
                
                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        menuRetorno

enviarMGeral :: IO()
enviarMGeral = do
        --Identificação do Usuário
        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int

        let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))
        let projetosNoSistema = getTodosProjetos "Database/projetos.json"

        case (usuarioNoSistema, projetosNoSistema) of
                (Just usuario, projetos )-> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                if (usuarioEstaEmAlgumProjeto idUsuario projetos || ehGerente idUsuario projetos)then do
                                        --Deve-se listar os projetos em do usuário e pedir que indique o id para mandar a mensagem.
                                        let projetos_user= listarProjetosDoUsuario idUsuario projetos
                                        putStrLn projetos_user
                                        putStrLn "Digite o id do projeto que deseja enviar a mensagem para todos os membros"
                                        idEscolhido <- readLn :: IO Int
                                        putStrLn "Digite a mensagem que será enviada para todos os membros do projetos"
                                        mensagem <- getLine
                                        salvarCaixadeMensagem "Database/mensagens.json" idEscolhido (nome usuario)mensagem
                                        menuRetorno
                                else do
                                        putStrLn "Não está em projeto !"
                                --putStrLn 
                        else do
                             putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                      ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                      ++ ".------------------------------------------------------------." ++ "\n"
                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"


enviarMPrivada:: IO()
enviarMPrivada = do
        --Identificação do Usuário
        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int

        let usuarios = getUsuarios "Database/usuarios.json"
        let usuarioNoSistema = (getUsuario idUsuario usuarios)

        case (usuarioNoSistema) of
                (Just usuario)-> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                putStrLn "Usuários ativos:\n"
                                aplicarImprimirUsuario (getUsuarios "Database/usuarios.json")
                                putStrLn "Digite o id usuário que deseja enviar uma mensagem privada"
                                id_destinatario <- readLn :: IO Int
                                putStrLn "Digite a mensagem a ser enviada"
                                mensagem <- getLine
                                salvarCaixadeMensagem "Database/mensagens.json" id_destinatario (nome usuario) mensagem
                                menuRetorno

                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                        ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                        ++ ".------------------------------------------------------------." ++ "\n"
                                menuRetorno
                                
                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        menuRetorno

menuRetorno :: IO()
menuRetorno = do
                putStrLn $ ".------------------------------------------------------------------." ++ "\n"
                        ++ "|         Deseja voltar ao Menu Principal ou ao CHAT  ?             |" ++ "\n"
                        ++ "|         C - MenuChat                                              |" ++ "\n"
                        ++ "|         P - MenuPrincipal                                         |" ++ "\n"
                        ++ "|         S - Sair do sistema                                       |" ++ "\n"
                        ++ ".------------------------------------------------------------------." ++ "\n"
                option <- getLine
                let lowerOption = map toLower option
                
                case lowerOption of
                        "c" -> menuChat
                        "p" -> menuPrincipal
                        "s" -> sairDoSistema
                        _  -> erroMenuPrincipal 
