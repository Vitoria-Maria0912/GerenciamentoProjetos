module Menus.MenuPublico where

import System.Info (os)
import System.Process (system)
import Data.Char (toLower)
import Data.Maybe
import Controllers.Usuario
import Controllers.Projeto
import Controllers.Atividades

-- | Limpa a tela, deixando apenas o atual comando
clearScreen :: IO ()
clearScreen = do
    case os of
        "linux" -> do
            _ <- system "clear"
            return ()
        "mingw32" -> do
            _ <- system "cls"
            return ()
        _ -> return ()

-- | Exibe erro e retorna ao menu
erroMenuPublico :: IO()
erroMenuPublico =  do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuPublicoProjeto

-- | Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :: IO()
menuPublicoProjeto = do 

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                    Menu Projeto                          |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                 Selecione uma opção:                     |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|            L - Listar projetos cadastrados               |" ++ "\n"
            ++ "|            I - Iniciar uma atividade                     |" ++ "\n"
            ++ "|            F - Finalizar uma atividade                   |" ++ "\n"
            ++ "|            V - Visualizar atividades do projeto          |" ++ "\n"
            ++ "|            A - Visualizar status de uma atividade        |" ++ "\n"
            ++ "|            O - Dar feedback em uma atividade             |" ++ "\n"
            ++ "|            S - Sair do sistema                           |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "l" -> do
                visualizarProjetos
                menuPublicoProjeto
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criaFeedback
        "s" -> sairDoSistema
        _   -> erroMenuPublico

-- | Sai do sistema
sairDoSistema :: IO()
sairDoSistema = do
    clearScreen
    putStrLn $ "\n" ++ ".----------------------------------------------------------." ++ "\n"
                    ++ "|            Você saiu do sistema! Até a próxima!          |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"

-- | Iniciar uma atividade
comecarAtividade :: IO()
comecarAtividade = do

    putStrLn $ "Começar atividade: \n\n"
            ++ "Digite seu ID:"
    idUsuario <- readLn :: IO Int
    
    let usuariosDoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))

    if isJust(usuariosDoSistema) then do
        putStrLn "Digite o ID da atividade que deseja começar:"
        idAtividade <- readLn :: IO Int
        let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

        case (atividadeDoSistema) of
                Just atividade -> do
                        if status atividade == "Não atribuída!" then do
                                let statusAtividade = mudaStatus atividade "Pendente..."
                        --     adicionaAtividadeAoUsuario idAtividade usuario
                                putStrLn $ "\n" ++ "Título: " ++ titulo atividade ++ "\n"
                                                ++ "Descrição: " ++ descricao atividade ++ "\n"
                                                ++ "Status: " ++ status statusAtividade

                        else do
                                putStrLn "Esta atividade já está em andamento!"

                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        comecarAtividade
    else do
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        comecarAtividade

-- | Finaliza uma atividade
finalizarAtividade :: IO()
finalizarAtividade = do
    
    putStrLn $ "Finalizar atividade: \n\n"
            ++ "Digite seu ID:"
    idUsuario <- readLn :: IO Int

    let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))

    case (usuarioNoSistema) of
        Just usuario -> do
                putStrLn "Digite o ID da atividade que deseja finalizar:"
                idAtividade <- readLn :: IO Int
                let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

                case (atividadeDoSistema) of
                    Just atividade -> do
                        let statusAtividade = mudaStatus atividade "Concluída"
                        -- adicionaAtividadeAoUsuario idAtividade usuario
                        putStrLn $ "\n" ++ "Título: " ++ titulo atividade ++ "\n"
                                        ++ "Descrição: " ++ descricao atividade ++ "\n"
                                        ++ "Status: " ++ status statusAtividade

                    Nothing -> do
                            clearScreen
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
                            finalizarAtividade
        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                finalizarAtividade

-- | Mostra o status de uma atividade
statusAtividade :: IO()
statusAtividade = do

    putStrLn "Digite o ID da atividade que deseja visualizar o status:"
    idAtividade <- readLn :: IO Int

    let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

    case (atividadeDoSistema) of
        Just atividade -> do
            let statusAtividade = getStatus atividade 
            putStrLn $ "\n" ++ "Título: " ++ titulo atividade ++ "\n"
                            ++ "Descrição: " ++ descricao atividade ++ "\n"
                            ++ "Status: " ++ statusAtividade

        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                statusAtividade

-- | Função para visualizar atividades do projeto
visualizarAtividades :: IO()
visualizarAtividades = do

    putStrLn $ "Visualizar atividade: \n\n"
            ++ "Digite o ID do projeto:"
            
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos "Database/projetos.json"
    let projetoNoSistema = getProjeto idProjeto projetosDoSistema

    case projetoNoSistema of
            Just projeto -> do
                clearScreen
                let atividadesCadastradas = (getTodasAtividades "Database/atividades.json")
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "            Estas são as atividades do projeto:             " ++ "\n"
                mapM_ imprimeAtividadesDoProjeto (getAtividadesDoProjeto (atividades projeto) atividadesCadastradas)
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                
                menuPublicoProjeto

            Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                menuPublicoProjeto

-- | Função para criar feedback
criaFeedback :: IO ()
criaFeedback = do

    putStrLn $ "Comente sobre uma atividade que você criou ou foi designado: \n\n"
            ++ "Digite seu ID:"
    idUsuario <- readLn :: IO Int

    let usuariosCadastrados = (getUsuarios "Database/usuarios.json")
    let usuarioNoSistema = (getUsuario idUsuario usuariosCadastrados)

    case (usuarioNoSistema) of
        Just usuario -> do
                putStrLn "Digite o ID da atividade que deseja dar Feedback:"
                idAtividade <- readLn :: IO Int

                let projetosCadastrados = (getTodosProjetos "Database/projetos.json")
                let atividadesCadastradas = (getTodasAtividades "Database/atividades.json")
                let atividadeDoSistema = (getAtividade idAtividade atividadesCadastradas)

                case (atividadeDoSistema) of
                    Just atividade -> do
                        if (ehGerente idUsuario projetosCadastrados) || (ehMembroResponsavel idUsuario atividadesCadastradas) then do
                            putStrLn "Escreva um breve comentário sobre a atividade:"
                            comentario <- getLine
                            criarFeedbacks "Database/atividades.json" idAtividade comentario
                        else do
                            clearScreen
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|     Você não está autorizado a realizar esta ação!       |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"

                    Nothing -> do
                            clearScreen
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
                            criaFeedback
        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                criaFeedback

-- | Função para visualizar projetos
visualizarProjetos :: IO ()
visualizarProjetos = do
  clearScreen

  let projetos = getTodosProjetos "Database/projetos.json"
  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "            Estes são os projetos no sistema:               " ++ "\n"
  mapM_ imprimirProjetos projetos
  putStrLn $ ".----------------------------------------------------------." ++ "\n"
