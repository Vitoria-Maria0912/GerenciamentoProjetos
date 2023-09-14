module Menus.MenuGerente where

import System.Random
import Data.Maybe
import System.Exit (exitSuccess)
import Controllers.Atividades
import Controllers.Projeto
import Menus.MenuPublico
import Data.Char (toLower)


-- Exibe erro e retorna ao menu
erroMenuGerente :: IO()
erroMenuGerente = do
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
    menuRestritoProjeto

-- Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :: IO()
menuRestritoProjeto = do 

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                   Menu Projeto                           |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                Selecione uma opção:                      |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|           P - Remover projeto                            |" ++ "\n"
            ++ "|           B - Visualizar banco de atividades             |" ++ "\n"
            ++ "|           C - Criar uma atividade                        |" ++ "\n"
            ++ "|           R - Remover uma atividade                      |" ++ "\n"
            ++ "|           I - Iniciar uma atividade                      |" ++ "\n"
            ++ "|           F - Finalizar uma atividade                    |" ++ "\n"
            ++ "|           V - Visualizar atividades do projeto           |" ++ "\n"
            ++ "|           A - Visualizar status de uma atividade         |" ++ "\n"
            ++ "|           G - Gerenciar membros do projeto               |" ++ "\n"
            ++ "|           S - Sair do sistema                            |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"
            
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "p" -> deletarProjeto
        -- "b" -> bancoDeAtividades
        "c" -> criaAtividade
        "r" -> deletaAtividade 
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        -- "v" -> visualizarAtividades
        "a" -> statusAtividade
        -- "g" -> gerenciarMembros
        "s" -> sairDoSistema
        _   -> erroMenuGerente

-- Função do menu para remover um projeto pelo ID
deletarProjeto :: IO ()
deletarProjeto = do

  let projectFilePath = "Database/projetos.json"
  
  putStrLn $ "Remover Projeto: \n\n" 
           ++ "Digite o ID do projeto que deseja excluir:"
  idProjeto <- readLn :: IO Int

  -- Ler projetos do arquivo
  let projetoNoSistema = (getProjeto idProjeto (getTodosProjetos projectFilePath))
  
  -- Verificar se o projeto com o ID especificado existe
  case (projetoNoSistema) of
    Just projeto -> do
      removerProjeto projectFilePath idProjeto
      putStrLn $ ".------------------------------------------------------------." ++ "\n"
               ++ "|              Projeto removido com sucesso!                |" ++ "\n"
               ++ ".------------------------------------------------------------." ++ "\n"
      menuRestritoProjeto
    Nothing -> do
      putStrLn $ ".-------------------------------------------------------------." ++ "\n"
               ++ "|   Projeto não encontrado! Verifique o ID e tente novamente  |" ++ "\n"
               ++ ".-------------------------------------------------------------." ++ "\n"
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
                            ++ "|            Falha no cadastro! Tente novamente.           |" ++ "\n"
                            ++ ".----------------------------------------------------------." ++ "\n"
                    criaAtividade
                    
                Nothing -> do
                        criarAtividade "Database/atividades.json" titulo descricao idProjeto idAtividade Nothing Nothing
                        -- ADICIONAR DO PROJETO
                        putStrLn $ "Tarefa criada com sucesso! O ID dessa atividade é \n"  ++ show(idAtividade)

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            Falha no cadastro! Tente novamente.           |" ++ "\n"
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
        Just _ ->  do
                -- DELETAR DO PROJETO
                deletarAtividade "Database/atividades.json" idAtividade
                putStrLn $ "\n Atividade de ID " ++ show(idAtividade) ++ " removida do projeto com sucesso!"

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|            Falha ao remover! Tente novamente.           |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                deletaAtividade
            
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
