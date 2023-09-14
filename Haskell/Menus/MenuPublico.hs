module Menus.MenuPublico where

import System.Info (os)
import System.Process (system)
import Controllers.Atividades
import Data.Char (toLower)
import Controllers.Usuario
import Controllers.Projeto
import Controllers.Atividades

-- Exibe erro e retorna ao menu
erroMenuPublico :: IO()
erroMenuPublico =  do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuPublicoProjeto

-- Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :: IO()
menuPublicoProjeto = do 

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                    Menu Projeto                          |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                 Selecione uma opção:                     |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
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

        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        -- "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criaFeedback
        "s" -> sairDoSistema
        _   -> erroMenuPublico

-- Sai do sistema
sairDoSistema :: IO()
sairDoSistema = do
    clearScreen
    putStrLn $ "\n" ++ ".----------------------------------------------------------." ++ "\n"
                    ++ "|            Você saiu do sistema! Até a próxima!          |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"


-- Inicia uma atividade
comecarAtividade :: IO()
comecarAtividade = do

    putStrLn $ "Começar atividade: \n\n"
            ++ "Digite seu ID:"
    idUsuario <- readLn :: IO Int
    
    let usuariosDoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))

    case (usuariosDoSistema) of
        Just usuario -> do
                putStrLn "Digite o ID da atividade que deseja começar:"
                idAtividade <- readLn :: IO Int
                let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

                case (atividadeDoSistema) of
                    Just atividade -> do
                        if status atividade == "Não atribuída!" then do
                            let statusAtividade = mudaStatus atividade "Pendente..."
                            -- ADICIONAR ÁS ATIVIDADES DO USUÁRIO
                            putStrLn $ "Título: " ++ titulo atividade ++ "\n"
                                    ++ "Descrição: " ++ descricao atividade ++ "\n"
                                    ++ "Status: " ++ status statusAtividade

                        else do
                            putStrLn "Esta atividade já está em andamento!"

                    Nothing -> do
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
                            comecarAtividade
        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                comecarAtividade

    

-- Finaliza uma atividade
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
                        -- ADICIONAR ÁS ATIVIDADES DO USUÁRIO
                        putStrLn $ "Título: " ++ titulo atividade ++ "\n"
                                ++ "Descrição: " ++ descricao atividade ++ "\n"
                                ++ "Status: " ++ status statusAtividade

                    Nothing -> do
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
                            finalizarAtividade
        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                finalizarAtividade

-- Mostra o status de uma atividade
statusAtividade :: IO()
statusAtividade = do

    putStrLn "Digite o ID da atividade que deseja visualizar o status:"
    idAtividade <- readLn :: IO Int

    let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

    case (atividadeDoSistema) of
        Just atividade -> do
            let statusAtividade = getStatus atividade 
            putStrLn $ "Título: " ++ titulo atividade ++ "\n"
                    ++ "Descrição: " ++ descricao atividade ++ "\n"
                    ++ "Status: " ++ statusAtividade

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                statusAtividade


-- visualizarAtividades :: IO()
-- visualizarAtividades = do 

--     putStrLn "Digite o ID do projeto que deseja visualizar as atividade:"
--     idProjeto <- readLn :: IO Int

--     -- será possível ver apenas o nome ou, por exemplo, a quantidade de membros
--     -- em cada atividade?

--     -- let projeto = Projeto.getProjeto idProjeto
   
--     putStrLn "projeto.exibeAtividades"


-- Função para criar feedback
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
                putStrLn "Escreva um breve comentário sobre a atividade:"
                comentario <- getLine
                let projetosCadastrados = (getTodosProjetos "Database/projetos.json")
                let atividadesCadastradas = (getTodasAtividades "Database/atividades.json")
                let atividadeDoSistema = (getAtividade idAtividade atividadesCadastradas)
                case (atividadeDoSistema) of
                    Just atividade -> do
                        if (ehGerente idUsuario projetosCadastrados) || (ehMembroResponsavel idUsuario atividadesCadastradas) then do
                            criarFeedbacks "Database/atividades.json" idAtividade comentario
                        else
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|     Você não está autorizado a realizar esta ação!       |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"

                    Nothing -> do
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
                            criaFeedback
        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                criaFeedback

-- Limpa a tela, deixando apenas o atual comando
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