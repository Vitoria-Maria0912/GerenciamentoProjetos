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
import Menus.MenuGerente 
import Menus.MenuPublico 


-- | Exibe erro e retorna ao menu
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuPrincipal

-- | Exibe erro e retorna ao menuChat
erroMenuChat :: IO()
erroMenuChat = do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    threadDelay 1000000
    menuChat

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

    idUsuario <- randomRIO (1000, 9999 :: Int)

    let usuarioNoSistema = getUsuario idUsuario (getUsuarios userFilePath)

    case usuarioNoSistema of
      Just _ -> do
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|             Falha no cadastro! Tente novamente.          |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"

      Nothing -> do
        salvarUsuario userFilePath idUsuario nome senha []
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "        Usuário cadastrado com sucesso! Seu ID é " ++ show idUsuario ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPrincipal

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
          
      else do
          clearScreen
          putStrLn $ ".------------------------------------------------------------." ++ "\n"
                  ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                  ++ ".------------------------------------------------------------." ++ "\n"

    Nothing -> do
          clearScreen
          putStrLn $ ".-----------------------------------------------------------." ++ "\n"
                  ++ "|              ID inexistente! Tente novamente!             |" ++ "\n"
                  ++ ".-----------------------------------------------------------." ++ "\n"
  retornoMenuPrincipal      

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

        idProjeto <- randomRIO (100, 999 :: Int)

        let projectFilePath = "Database/projetos.json"
        let projetoNoSistema = getProjeto idProjeto (getTodosProjetos projectFilePath)

        if isJust(projetoNoSistema) then do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|             Falha no cadastro! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
        else do
                criaProjeto projectFilePath idProjeto nomeProjeto descricaoProjeto idUsuario [] []
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "  Projeto criado com sucesso! O ID do seu projeto é: " ++ show idProjeto ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"

        else do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPrincipal

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
        retornoMenuPrincipal

    else do
        clearScreen
        let projetos = getTodosProjetos projectFilePath
        let gerente = ehGerente idUsuario projetos

        if gerente then menuRestritoProjeto
        else menuPublicoProjeto

-- | Entra no chat
menuChat :: IO ()
menuChat = do
  
  putStrLn $ ".-----------------------------------------------------------------------------." ++ "\n"
          ++ "|                           Bem-vindo ao Chat!                                |" ++ "\n"
          ++ "|                                                                             |" ++ "\n"
          ++ "|     Envie mensagens entre membros do seu projeto e usuários do sistema!     |" ++ "\n"
          ++ "|                                                                             |" ++ "\n"
          ++ "|                          Selecione uma opção:                               |" ++ "\n"
          ++ "|                                                                             |" ++ "\n"
          ++ "|              C - Visualizar mensagens gerais de um projeto                  |" ++ "\n"
          ++ "|              H - Visualizar mensagens privadas                              |" ++ "\n"
          ++ "|              A - Enviar mensagem geral para membros do projeto              |" ++ "\n"
          ++ "|              T - Enviar mensagem privada                                    |" ++ "\n"
          ++ "|              S - Sair do sistema                                            |" ++ "\n"
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
  clearScreen

-- | Mostra as mensagens por projeto que o membro faz parte
visualizarMensagensGerais :: IO()
visualizarMensagensGerais = do

        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "              Mensagens gerais de um projeto:               " ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"

        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int
      
        let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))
        let projetos = getTodosProjetos "Database/projetos.json"
        let mensagen = getMensagens "Database/mensagens.json"

        case (usuarioNoSistema, projetos) of
                (Just usuario, projetos) -> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                if (usuarioEstaEmAlgumProjeto idUsuario projetos || ehGerente idUsuario projetos)then do
                                        clearScreen
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

                                else do
                                        clearScreen
                                        putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                                ++ "|         Usuário não está cadastrado em nenhum projeto.     |" ++ "\n"
                                                 ++ ".-----------------------------------------------------------." ++ "\n"
                                
                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                        ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                        ++ ".------------------------------------------------------------." ++ "\n"
                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
        retornoMenuPrincipal

-- | Mostra as mensagens por usuário
visualizarMensagensPrivadas :: IO()
visualizarMensagensPrivadas = do

        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "              Mensagens privadas de um usuário:             " ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"

        putStrLn "Digite seu ID:"
        idUsuario <- readLn:: IO Int
        let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))
        let projetos = getTodosProjetos "Database/projetos.json"
        case (usuarioNoSistema, projetos) of
                (Just usuario, projetos) -> do
                        putStrLn "Digite sua senha:"
                        senha <- getLine
                        if (verificaSenhaUsuario usuario senha) then do
                                putStrLn "Mensagens Privadas:"
                                let mensagen = getMensagens "Database/mensagens.json"
                                putStrLn "\nCarregando...\n  "
                                threadDelay 100000
                                exibeMensagens mensagen idUsuario
                                threadDelay 100000
                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                        ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                        ++ ".------------------------------------------------------------." ++ "\n"
                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
        retornoMenuPrincipal


-- | Envia uma mensagem para os membros de um projeto
enviarMGeral :: IO()
enviarMGeral = do

        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "      Enviar mensagem para todos os membros do projeto:     " ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"

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
                                        let projetos_user= listarProjetosDoUsuario idUsuario projetos
                                        putStrLn projetos_user
                                        putStrLn "Digite o id do projeto que deseja enviar a mensagem para todos os membros"
                                        idEscolhido <- readLn :: IO Int
                                        putStrLn "Digite a mensagem que será enviada para todos os membros do projetos"
                                        mensagem <- getLine
                                        salvarCaixadeMensagem "Database/mensagens.json" idEscolhido (nome usuario)mensagem
                                        putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                                ++ "|                Mensagem enviada com sucesso !              |" ++ "\n"
                                                ++ ".------------------------------------------------------------." ++ "\n"
                                        
                                else do
                                        putStrLn $ ".---------------------------------------------------------------." ++ "\n"
                                                ++ "|           Este Usuário não é membro de nenhum projeto!        |" ++ "\n"
                                                ++ ".---------------------------------------------------------------." ++ "\n"
                        else do
                             clearScreen
                             putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                      ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                      ++ ".------------------------------------------------------------." ++ "\n"

                (Nothing, _) -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
        retornoMenuPrincipal

-- | Envia mensagem para um usuário do sistema
enviarMPrivada:: IO()
enviarMPrivada = do

        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "                Enviar mensagem para um usuário:            " ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"

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
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                        ++ "|                Mensagem enviada com sucesso !              |" ++ "\n"
                                        ++ ".------------------------------------------------------------." ++ "\n"
                        else do
                                clearScreen
                                putStrLn $ ".------------------------------------------------------------." ++ "\n"
                                        ++ "|              Senha incorreta! Tente novamente.             |" ++ "\n"
                                        ++ ".------------------------------------------------------------." ++ "\n"
                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID inexistente! Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
        retornoMenuPrincipal

-- | Retorna ao menu principal ou sai do sistema
retornoMenuPrincipal :: IO()
retornoMenuPrincipal = do

        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "| Você deseja voltar ao menu principal ou sair do sistema? |" ++ "\n"
                ++ "|                                                          |" ++ "\n"
                ++ "|                   M - Menu principal                     |" ++ "\n"
                ++ "|                   S - Sair do sistema                    |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        opcao <- getLine
        let lowerOption = map toLower opcao
        
        case lowerOption of
                "m" -> do
                        clearScreen
                        menuPrincipal
                "s" -> sairDoSistema
                _   -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        retornoMenuPrincipal 
