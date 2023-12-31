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

-- | Sai do sistema
sairDoSistema :: IO()
sairDoSistema = do
    clearScreen
    putStrLn $ "\n" ++ ".----------------------------------------------------------." ++ "\n"
                    ++ "|            Você saiu do sistema! Até a próxima!          |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"

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
                retornoMenuPublico
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criaFeedback
        "s" -> sairDoSistema
        _   -> erroMenuPublico

-- | Iniciar uma atividade
comecarAtividade :: IO()
comecarAtividade = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                    Começar atividade:                        " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite seu ID:\n"
    idUsuario <- readLn :: IO Int
    
    let usuariosDoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))

    case (usuariosDoSistema) of
        Just usuario -> do
                putStrLn "Digite o ID da atividade que deseja começar:"
                idAtividade <- readLn :: IO Int
                let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json"))

                case (atividadeDoSistema) of
                    Just atividade -> do
                        
                        if (atividadeEstaAtribuida idAtividade usuario) then do
                        
                                if (status atividade) == "Não atribuída!" then do
                                        clearScreen
                                        editStatus "Database/bancoDeAtividades.json" idAtividade "Pendente..."
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "                Você começou a atividade (ID: " ++ show(idAtividade) ++ ")\n"
                                                ++ "                                                            " ++ "\n"
                                                ++ "    Título: " ++ titulo atividade ++ "\n"
                                                ++ " Descrição: " ++ descricao atividade ++ "\n"
                                                ++ "    Status: Pendente..." ++ "\n"
                                
                                else if (status atividade) == "Concluída!" then do
                                        clearScreen
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "|             Esta atividade já foi concluída!             |" ++ "\n"
                                                ++ ".----------------------------------------------------------." ++ "\n"
                                else do
                                        clearScreen
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "|           Esta atividade já está em andamento!           |" ++ "\n"
                                                ++ ".----------------------------------------------------------." ++ "\n"
                        
                        else do 
                                clearScreen
                                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                        ++ "|         Você não está atribuído a essa atividade!        |" ++ "\n"
                                        ++ ".----------------------------------------------------------." ++ "\n"
                                
                    Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"

        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPublico                                                  

-- | Finaliza uma atividade
finalizarAtividade :: IO()
finalizarAtividade = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                    Finalizar atividade:                    " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    
    putStrLn "Digite seu ID:\n"
    idUsuario <- readLn :: IO Int

    let usuarioNoSistema = (getUsuario idUsuario (getUsuarios "Database/usuarios.json"))

    case (usuarioNoSistema) of
        Just usuario -> do
                putStrLn "Digite o ID da atividade que deseja finalizar:"
                idAtividade <- readLn :: IO Int
                let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json"))

                case (atividadeDoSistema) of
                    Just atividade -> do

                        if (atividadeEstaAtribuida idAtividade usuario) then do
                        
                                if (status atividade) == "Concluída!" then do
                                        clearScreen
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "|             Esta atividade já foi concluída!             |" ++ "\n"
                                                ++ ".----------------------------------------------------------." ++ "\n"
                                else do
                                        clearScreen
                                        editStatus "Database/bancoDeAtividades.json" idAtividade "Concluída!"
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "               Você finalizou a atividade (ID: " ++ show(idAtividade) ++ ")\n"
                                                ++ "                                                            " ++ "\n"
                                                ++ "    Título: " ++ titulo atividade ++ "\n"
                                                ++ " Descrição: " ++ descricao atividade ++ "\n"
                                                ++ "    Status: Concluída!" ++ "\n"
                        
                        else do 
                                clearScreen
                                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                        ++ "|         Você não está atribuído a essa atividade!        |" ++ "\n"
                                        ++ ".----------------------------------------------------------." ++ "\n"

                    Nothing -> do
                            clearScreen
                            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                    ++ "|              ID incorreto! Tente novamente.              |" ++ "\n"
                                    ++ ".----------------------------------------------------------." ++ "\n"
        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPublico

-- | Mostra o status de uma atividade
statusAtividade :: IO()
statusAtividade = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                 Mostrar status da atividade:               " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "\nDigite o ID da atividade:\n"
    idAtividade <- readLn :: IO Int

    let atividadeDoSistema = (getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json"))

    case (atividadeDoSistema) of
        Just atividade -> do
            let statusAtividade = getStatus atividade 
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "           Visualizar status da atividade (ID: " ++ show(idAtividade) ++ ")\n"
                    ++ "                                                            " ++ "\n"
                    ++ "    Título: " ++ titulo atividade ++ "\n"
                    ++ " Descrição: " ++ descricao atividade ++ "\n"
                    ++ "    Status: " ++ statusAtividade ++ "\n"

        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPublico

-- | Função para visualizar atividades do projeto
visualizarAtividades :: IO()
visualizarAtividades = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "              Visualizar atividades do projeto:             " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    visualizarProjetos

    putStrLn "Digite o ID do projeto:"
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos "Database/projetos.json"
    let projetoNoSistema = getProjeto idProjeto projetosDoSistema

    case projetoNoSistema of
            Just projeto -> do
                clearScreen
                let atividadesCadastradas = (getTodasAtividades "Database/bancoDeAtividades.json")
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "       Estas são as atividades do projeto (ID: " ++ show(idProjeto) ++ ")\n"
                mapM_ imprimeAtividadesDoProjeto (getAtividadesDoProjeto (atividades projeto) atividadesCadastradas)
                putStrLn $ ".----------------------------------------------------------." ++ "\n"

            Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPublico

-- | Função para criar feedback
criaFeedback :: IO ()
criaFeedback = do

    putStrLn $ ".-------------------------------------------------------------." ++ "\n"
            ++ "  Comente sobre uma atividade que você criou ou foi designado:      " ++ "\n"
            ++ ".-------------------------------------------------------------." ++ "\n"

    putStrLn "Digite seu ID:\n"
    idUsuario <- readLn :: IO Int

    let usuariosCadastrados = (getUsuarios "Database/usuarios.json")
    let usuarioNoSistema = (getUsuario idUsuario usuariosCadastrados)

    case (usuarioNoSistema) of
        Just usuario -> do
                putStrLn "\nDigite o ID da atividade que deseja dar Feedback:"
                idAtividade <- readLn :: IO Int

                let projetosCadastrados = (getTodosProjetos "Database/projetos.json")
                let atividadesCadastradas = (getTodasAtividades "Database/bancoDeAtividades.json")
                let atividadeDoSistema = (getAtividade idAtividade atividadesCadastradas)

                case (atividadeDoSistema) of
                    Just atividade -> do
                        clearScreen
                        if (ehGerente idUsuario projetosCadastrados) || (ehMembroResponsavel (Just(idUsuario)) atividade) then do
                            putStrLn "\nEscreva um breve comentário sobre a atividade:"
                            comentario <- getLine
                            editFeedback "Database/bancoDeAtividades.json" idAtividade comentario True
                            putStrLn $ "\n" ++ ".----------------------------------------------------------." ++ "\n"
                                            ++ " Comentário adicionado com sucesso a atividade de ID " ++ show idAtividade ++ "\n"
                                            ++ ".----------------------------------------------------------." ++ "\n"
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
        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            ID inexistente! Tente novamente!              |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuPublico

-- | Função para visualizar projetos
visualizarProjetos :: IO ()
visualizarProjetos = do
  clearScreen
  putStrLn $ ".-----------------------------------------------------------." ++ "\n"
          ++ "            Estes são os projetos no sistema:               " ++ "\n"
  mapM_ imprimirProjetos (getTodosProjetos "Database/projetos.json")
  putStrLn $ ".-----------------------------------------------------------." ++ "\n"

-- | Retorna ao menu principal ou sai do sistema
retornoMenuPublico :: IO()
retornoMenuPublico = do
        
        putStrLn $ ".-----------------------------------------------------------." ++ "\n"
                ++ "| Você deseja voltar ao menu do projeto ou sair do sistema? |" ++ "\n"
                ++ "|                                                           |" ++ "\n"
                ++ "|                 M - Menu de projetos                      |" ++ "\n"
                ++ "|                 S - Sair do sistema                       |" ++ "\n"
                ++ ".-----------------------------------------------------------." ++ "\n"
        opcao <- getLine
        let lowerOption = map toLower opcao
        
        case lowerOption of
                "m" -> menuPublicoProjeto
                "s" -> sairDoSistema
                _   -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n" 
                        retornoMenuPublico
