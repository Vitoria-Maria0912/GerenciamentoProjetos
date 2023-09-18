module Menus.MenuGerente where

import System.Random
import Data.Maybe
import System.Exit (exitSuccess)
import Controllers.Atividades
import Controllers.Projeto
import Controllers.Usuario
import Menus.MenuPublico
import Data.Char (toLower)


-- | Exibe erro e retorna ao menu
erroMenuGerente :: IO()
erroMenuGerente = do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuRestritoProjeto

-- | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :: IO()
menuRestritoProjeto = do 

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                   Menu Projeto                           |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                Selecione uma opção:                      |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|           P - Remover projeto                            |" ++ "\n"
            ++ "|           G - Gerenciar membros do projeto               |" ++ "\n"
            ++ "|           B - Visualizar banco de atividades             |" ++ "\n"
            ++ "|           C - Criar uma atividade                        |" ++ "\n"
            ++ "|           R - Remover uma atividade                      |" ++ "\n"
            ++ "|           I - Iniciar uma atividade                      |" ++ "\n"
            ++ "|           F - Finalizar uma atividade                    |" ++ "\n"
            ++ "|           V - Visualizar atividades do projeto           |" ++ "\n"
            ++ "|           A - Visualizar status de uma atividade         |" ++ "\n"
            ++ "|           O - Dar feedback em uma atividade              |" ++ "\n"
            ++ "|           S - Sair do sistema                            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
            
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "p" -> deletarProjeto
        -- "g" -> gerenciarMembros
        -- "b" -> bancoDeAtividades
        "c" -> criaAtividade
        "r" -> deletaAtividade 
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        -- "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criaFeedback
        "s" -> sairDoSistema
        _   -> erroMenuGerente

-- | Função do menu para remover um projeto pelo ID
deletarProjeto :: IO ()
deletarProjeto = do

  let projectFilePath = "Database/projetos.json"

  putStrLn $ "Remover Projeto: \n\n" 
          ++ "Digite seu ID: "
  idUsuario <- readLn :: IO Int

  let usuarios = getUsuarios "Database/usuarios.json"
  let usuarioNoSistema = (getUsuario idUsuario usuarios)

  if isJust (getUsuario idUsuario usuarios) then do 

        putStrLn "Digite o ID do projeto que deseja excluir:"
        idProjeto <- readLn :: IO Int

        let projetoNoSistema = (getProjeto idProjeto (getTodosProjetos projectFilePath))
  
        case (projetoNoSistema) of
                Just projeto -> do
                        if (idGerente projeto == idUsuario) then do
                                removerProjeto projectFilePath idProjeto
                                clearScreen
                                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                        ++ "|              Projeto removido com sucesso!               |" ++ "\n"
                                        ++ ".----------------------------------------------------------." ++ "\n"
                        else do
                                clearScreen
                                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                        ++ "|      Você não tem permissão para realizar esta ação.     |" ++ "\n"
                                        ++ ".----------------------------------------------------------." ++ "\n"
                        menuRestritoProjeto

                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        deletarProjeto
  else do
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|             ID inexistente! Tente novamente!             |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        deletarProjeto

-- Cria uma atividade em um projeto
criaAtividade :: IO()
criaAtividade = do

    -- exibir bancoDeAtividades

    putStrLn $ "Criar atividade: \n\n"
            ++ "Digite o ID do projeto que deseja adicionar uma atividade: "
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos  "Database/projetos.json"

    let projetos = (getProjeto idProjeto projetosDoSistema)

    case (projetos) of
        Just projeto -> do
            putStrLn "Digite um título para sua atividade: "
            titulo <- getLine

            putStrLn "Descreva, brevemente, o que se deve realizar para concluir esta atividade."
            descricao <- getLine

            idAtividade <- randomRIO (00000, 99999 :: Int)

            let atividadeNoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

            case (atividadeNoSistema) of
                Just _ ->  do
                    putStrLn $ ".----------------------------------------------------------." ++ "\n"
                            ++ "|            Falha no cadastro! Tente novamente!           |" ++ "\n"
                            ++ ".----------------------------------------------------------." ++ "\n"
                    criaAtividade
                    
                Nothing -> do
                        adicionaAtividadeAoProjeto "Database/atividades.json" titulo descricao idProjeto idAtividade Nothing Nothing
                        putStrLn $ "Atividade criada com sucesso!  ID da atividade é: " ++ show(idAtividade)
                        menuRestritoProjeto

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                criaAtividade
            
-- Remove uma atividade de um projeto
deletaAtividade :: IO()
deletaAtividade = do

--  exibir bancoDeAtividades

    putStrLn $ "Deletar atividade: \n\n"
            ++ "Digite o ID da atividade que deseja remover:"
    idAtividade <- readLn :: IO Int

    let atividadesNoSistema = (getAtividade idAtividade (getTodasAtividades "Database/atividades.json"))

    case (atividadesNoSistema) of
        Just atividade ->  do
                deletarAtividadeProjeto "Database/atividades.json" (idProjetoAtividade atividade) idAtividade
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|              Atividade removida com sucesso!             |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                menuRestritoProjeto

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|          Atividade inexistente! Tente novamente.         |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                deletaAtividade

-- | Função para visualizar atividades
visualizarAtividades :: IO()
visualizarAtividades = do
    let atividadesCadastradas = (getTodasAtividades "Database/atividades.json")
    mapM_ imprimirAtividades atividadesCadastradas
    menuPublicoProjeto

-- | Função que imprime as atividades para visualização
imprimirAtividades :: Atividade -> IO()
imprimirAtividades atividade = putStrLn $   "Título: " ++ (titulo atividade) ++ "\n" ++
                                            "Descrição: " ++ (descricao atividade) ++ "\n" ++
                                            "ID Projeto: " ++ show (idProjetoAtividade atividade) ++ "\n" ++
                                            "ID Atividade: " ++ show (idAtividade atividade) ++ "\n" ++
                                            "ID Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
                                            "Status: " ++ status atividade ++ "\n"



-- Visualizar todos os membros do projeto
-- gerenciarMembros :: IO()
-- gerenciarMembros = do

--     putStrLn $ "Gerenciamento de membros: \n\n"
--             ++ "Digite o ID do projeto:"
--     idProjeto <- readLn :: IO Int

--     let projetosDoSistema = lerProjetos "Database/projetos.json"

--     let projetos = (getProjeto idProjeto projetosDoSistema)

--     case (projetos) of
--         Just projeto -> do
--                         -- invoca função para visualizar membros do projeto, em Projetos.hs
--                         putStrLn "Membros do projeto:"
--                         -- imprime os membros do projeto

--                         putStrLn $ ".----------------------------------------------------------." ++ "\n" 
--                                 ++ "|                O que deseja fazer agora?                 |" ++ "\n"
--                                 ++ "|                                                          |" ++ "\n"
--                                 ++ "|                  Selecione uma opção:                    |" ++ "\n"
--                                 ++ "|                                                          |" ++ "\n"
--                                 ++ "|           D - Atribuir atividade a um membro             |" ++ "\n"
--                                 ++ "|           M - Adicionar membro ao projeto                |" ++ "\n"
--                                 ++ "|           R - Remover membro do projeto                  |" ++ "\n"
--                                 ++ "|           V - Voltar ao menu do projeto                  |" ++ "\n"
--                                 ++ ".----------------------------------------------------------." ++ "\n"

--                         option <- getLine
--                         let lowerOption = map toLower option
--                         case lowerOption of 

--                                 -- "a" -> atribuirMembro
--                                 -- "n" -> adicionaNovoMembro
--                                 -- "r" -> removeMembroProjeto
--                                 "v" -> menuRestritoProjeto
--                                 _   -> erroMenuGerente
--         Nothing -> do
--                 putStrLn $ ".----------------------------------------------------------." ++ "\n"
--                         ++ "|              ID inválido, tente novamente.               |" ++ "\n"
--                         ++ ".----------------------------------------------------------." ++ "\n"
--                 gerenciarMembros

-- Adiciona um novo membro a um projeto
-- adicionaNovoMembro :: IO()
-- adicionaNovoMembro = do

--     putStrLn $ "Adicionar novo membro: \n\n" 
--             ++ "Digite o ID do projeto:"
--     idProjeto <- getLine
--     putStrLn "Digite o ID do membro que deseja adicionar:"
--     idUsuario <- getLine

--     putStrLn "Membro adicionado com sucesso!"

-- -- Remover membro do projeto
-- removeMembroProjeto :: IO()
-- removeMembroProjeto = do

--     putStrLn $ "Remover membro do projeto: \n\n" 
--             ++ "Digite o ID do projeto:"
--     idProjeto <- getLine
--     putStrLn "Digite o ID do membro que deseja remover:"
--     idUsuario <- getLine
    
--     projetos <- lerProjetos "Database/projetos.json"

--     let projeto = getProjeto idProjeto projetos

--     -- projeto.removeMembroProjeto idProjeto usuario
--     putStrLn "Membro removido do projeto com sucesso!"


-- -- Atribuir membro a uma atividade
-- atribuirMembro :: IO()
-- atribuirMembro = do

--     putStrLn $ "Atribuir uma atividade a um membro: \n\n" 
--             ++ "Digite o ID da atividade:"
--     idAtividade <- getLine
--     putStrLn "Digite o ID do projeto que a atividade pertence:"
--     idProjeto <- getLine
--     putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
--     idMembroResponsavel <- getLine

--     -- let projeto = getProjeto id
--     -- let atividade = getAtividade idAtividade
--     -- Atividades.atribuirMembro atividade membroResponsavel

--     -- TEM QUE ARMAZENÁ-LOS

--     putStrLn "Membro atribuído à atividade com sucesso!"

-- -- Visualiza atividades cadastradas no sistema
-- bancoDeAtividades :: IO ()
-- bancoDeAtividades = do
--   putStrLn "Implementação em andamento."



----------------------------------------------------------------------------------------------------------------
--BANCO DE ATIVIDADES
-- Função para o menu de banco de atividades no menu do gerente
menuBancoDeAtividades :: IO ()
menuBancoDeAtividades = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "               Menu Banco de Atividades (Gerente):         " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
            ++ "|                Selecione uma opção:                      |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|           L - Listar atividades cadastradas               |" ++ "\n"
            ++ "|           A - Adicionar uma nova atividade                |" ++ "\n"
            ++ "|           C - Consultar uma atividade por ID              |" ++ "\n"
            ++ "|           F - Filtrar atividades por status               |" ++ "\n"
            ++ "|           V - Voltar ao menu principal                    |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
        "l" -> listarAtividades
        "a" -> adicionarAtividade
        "c" -> consultarAtividade
        "f" -> filtrarAtividades
        "v" -> return ()  -- Voltar ao menu principal do gerente
        _   -> putStrLn "Opção inválida." >> menuBancoDeAtividades


-- Função para listar atividades cadastradas
listarAtividades :: IO ()
listarAtividades = do
    atividades <- lerBancoDeAtividades
    clearScreen  -- Limpa a tela
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "               Atividades Cadastradas:                     " ++ "\n"
    mapM_ imprimirDetalhesAtividade atividades
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
    menuBancoDeAtividades

-- Função para criar uma nova atividade e adicioná-la ao banco
adicionarAtividade :: IO ()
adicionarAtividade = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                   Adicionar uma nova atividade:           " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    putStrLn "Digite o título da nova atividade: "
    titulo <- getLine
    putStrLn "Digite a descrição da nova atividade: "
    descricao <- getLine
    putStrLn "Digite o status da nova atividade: "
    status <- getLine

    -- Crie uma nova atividade
    let novaAtividade = Atividade
            { titulo = titulo
            , descricao = descricao
            , idAtividade = 0 -- Defina o ID da maneira que desejar
            , idMembroResponsavel = "ID do Responsável" -- Defina o ID do responsável
            , idProjetoAtividade = 0 -- Defina o ID do projeto
            , status = status
            , feedbacks = [] -- Inicialize com uma lista vazia de feedbacks
            }

    -- Chame a função para adicionar a atividade ao JSON
    adicionarAtividadeAoJSON novaAtividade
    putStrLn "Atividade adicionada com sucesso!"
    menuBancoDeAtividades


-- Função para consultar uma atividade por ID
consultarAtividade :: IO ()
consultarAtividade = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                Consultar uma atividade por ID:            " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    putStrLn "Digite o ID da atividade a ser consultada: "
    idAtividade <- readLn :: IO Int
    atividade <- consultarAtividadePorID idAtividade
    case atividade of
        Just a  -> do
            putStrLn $ "Atividade encontrada:"
            imprimirAtividade a
        Nothing -> putStrLn "Atividade não encontrada."
    menuBancoDeAtividades

-- Função para filtrar atividades por status
filtrarAtividades :: IO ()
filtrarAtividades = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "               Filtrar atividades por status:              " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    putStrLn "Digite o status para filtrar as atividades: "
    status <- getLine
    atividadesFiltradas <- filtrarAtividadesPorStatus status
    if null atividadesFiltradas
        then putStrLn "Nenhuma atividade encontrada com o status especificado."
        else do
            putStrLn $ "Atividades com o status '" ++ status ++ "':"
            mapM_ imprimirAtividade atividadesFiltradas
    menuBancoDeAtividades
