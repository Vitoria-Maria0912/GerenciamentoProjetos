module Menus.MenuGerente where

import System.Random
import Data.Maybe
import System.Exit (exitSuccess)
import Controllers.Atividades
import Controllers.Projeto
import Controllers.Usuario
import Menus.MenuPublico
import Data.Char (toLower)
import Text.Read (readMaybe)


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
        "g" -> gerenciarMembros
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
  let usuarioNoSistema = getUsuario idUsuario usuarios

  if isJust (getUsuario idUsuario usuarios) then do

        putStrLn "Digite o ID do projeto que deseja excluir:"
        idProjeto <- readLn :: IO Int

        let projetoNoSistema = getProjeto idProjeto (getTodosProjetos projectFilePath)

        case projetoNoSistema of
                Just projeto -> do
                        if idGerente projeto == idUsuario then do
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

    let projetos = getProjeto idProjeto projetosDoSistema

    case projetos of
        Just projeto -> do
            putStrLn "\nDigite um título para sua atividade: "
            titulo <- getLine

            putStrLn "\nDescreva, brevemente, o que se deve realizar para concluir esta atividade."
            descricao <- getLine

            idAtividade <- randomRIO (10010, 99999 :: Int)

            let atividadeNoSistema = getAtividade idAtividade (getTodasAtividades "Database/atividades.json")

            case atividadeNoSistema of
                Just _ ->  do
                    putStrLn $ ".----------------------------------------------------------." ++ "\n"
                            ++ "|            Falha no cadastro! Tente novamente!           |" ++ "\n"
                            ++ ".----------------------------------------------------------." ++ "\n"
                    criaAtividade

                Nothing -> do
                        editAtivDoProjeto "Database/projetos.json" idProjeto idAtividade True
                        criarAtividade "Database/atividades.json" titulo descricao idProjeto idAtividade Nothing []
                        -- chamar adicionarAtividade do banco de dados para adicionar essa atividade sem relacionar ao projeto e aumentar o banco de atividade
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                 ++ "        Atividade criada com sucesso! Seu ID é " ++ show idAtividade ++ "\n"
                                 ++ ".----------------------------------------------------------." ++ "\n"
                        menuRestritoProjeto

        Nothing -> do
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
                menuRestritoProjeto

-- Remove uma atividade de um projeto
deletaAtividade :: IO()
deletaAtividade = do

--  exibir bancoDeAtividades

    putStrLn $ "Deletar atividade: \n\n"
             ++ "Digite o ID do projeto que deseja remover uma atividade: "
    idProjeto <- readLn :: IO Int
    putStrLn "\nDigite o ID da atividade que deseja remover:"
    idAtividade <- readLn :: IO Int

    let atividadesNoSistema = getAtividade idAtividade (getTodasAtividades "Database/atividades.json")

    case atividadesNoSistema of
        Just atividade ->  do
                editAtivDoProjeto "Database/projetos.json" idProjeto idAtividade False
                deletarAtividade "Database/atividades.json" idAtividade
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

-- Visualizar todos os membros do projeto
gerenciarMembros :: IO ()
gerenciarMembros = do
    putStrLn $ "Gerenciamento de membros: \n\n"
             ++ "Digite o ID do projeto:"
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos "Database/projetos.json"
    let projetos = getProjeto idProjeto projetosDoSistema

    case projetos of
        Just projeto -> do
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                     ++ "|                O que deseja fazer agora?                 |" ++ "\n"
                     ++ "|                                                          |" ++ "\n"
                     ++ "|                  Selecione uma opção:                    |" ++ "\n"
                     ++ "|                                                          |" ++ "\n"
                     ++ "|           A - Atribuir atividade a um membro             |" ++ "\n"
                     ++ "|           N - Adicionar membro ao projeto                |" ++ "\n"
                     ++ "|           R - Remover membro do projeto                  |" ++ "\n"
                     ++ "|           V - Voltar ao menu do projeto                  |" ++ "\n"
                     ++ ".----------------------------------------------------------." ++ "\n"

            option <- getLine
            let lowerOption = map toLower option
            case lowerOption of
                "a" -> atribuirMembro
                "n" -> adicionaNovoMembro
                "r" -> removeMembroProjeto
                "v" -> menuRestritoProjeto
                _   -> erroMenuGerente
        Nothing -> do
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "|              ID inválido, tente novamente.               |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"
            menuRestritoProjeto -- coloquei esse porque se deixar gerenciarMembros fica em loop


-- visualizarMembros:: IO()
-- visualizarMembros = do
--         let usuarios = getUsuarios "Database/usuarios.json"
--         let membros = getMembrosDoProjeto projeto
--         putStrLn "Membros do projeto:"
--         putStrLn (imprimirListaDeIds membros)

-- Adiciona um novo membro a um projeto
adicionaNovoMembro :: IO ()
adicionaNovoMembro = do
  putStrLn "Adicionar novo membro: \n\nDigite o ID do projeto:"
  idProjeto <- readLn :: IO Int
  -- COLOCAR VERIFICACAO SE ESSE PROJETO ESTÁ NA LISTA DE PROJETOS
  putStrLn "Segue a lista de usuários disponíveis no sistema para adição no projeto \n"
  let usuarios = getUsuarios "Database/usuarios.json"
  mapM_ imprimirUsuario usuarios

  putStrLn $ ".---------------------------------------------------------." ++ "\n"
           ++"|          Digite o ID do membro que deseja adicionar:   |" ++ "\n"
           ++".---------------------------------------------------------." ++ "\n"
  idUsuario <- readLn :: IO Int


  let listaProjetos = getTodosProjetos "Database/projetos.json"
  let projeto = getProjeto idProjeto listaProjetos

  let membroNoProjeto = case projeto of
                            Just p -> membroEstaNoProjeto idUsuario p && idUsuario /= idGerente p
                            Nothing -> False

  (if membroNoProjeto then (do
          putStrLn $ ".---------------------------------------------------------." ++ "\n"
                    ++"|             Membro já está no projeto!                 |" ++ "\n"
                    ++".---------------------------------------------------------." ++ "\n") else (do
           editMembrosDoProjeto "Database/projetos.json" idProjeto idUsuario True
           putStrLn $ ".---------------------------------------------------------." ++ "\n"
                    ++"|             Membro adicionado com sucesso!              |" ++ "\n"
                    ++".---------------------------------------------------------." ++ "\n"))
           -- ADICIONAR CHAMADA PARA O MENU RESTRITO

-- Remover membro do projeto
removeMembroProjeto :: IO ()
removeMembroProjeto = do
  putStrLn $ "Remover membro do projeto: \n\n" 
          ++ "Digite o ID do projeto:"
  idProjeto <- readLn :: IO Int
  putStrLn "Digite o ID do membro que deseja remover:"
  idUsuario <- readLn :: IO Int

  let listaProjetos = getTodosProjetos "Database/projetos.json"
  let projeto = getProjeto idProjeto listaProjetos

  let membroNoProjeto = case projeto of
                          Just p -> membroEstaNoProjeto idUsuario p && idUsuario /= idGerente p
                          Nothing -> False

  if membroNoProjeto then do
    editMembrosDoProjeto "Database/projetos.json" idProjeto idUsuario False
    putStrLn $ ".---------------------------------------------------------." ++ "\n"
             ++ "|             Membro removido do projeto!                 |" ++ "\n"
             ++ ".---------------------------------------------------------." ++ "\n"
  else do
    putStrLn $ ".---------------------------------------------------------." ++ "\n"
             ++ "|             Membro não está no projeto!                 |" ++ "\n"
             ++ ".---------------------------------------------------------." ++ "\n"



-- Atribuir membro a uma atividade
atribuirMembro :: IO()
atribuirMembro = do

    putStrLn $ "Atribuir uma atividade a um membro: \n\n"
            ++ "Digite o ID da atividade:"
    idAtividade <- readLn :: IO Int
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- readLn :: IO Int
    putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
    idMembroResponsavel <- readLn :: IO Int

    let listaProjetos = getTodosProjetos "Database/projetos.json"
    let projeto = getProjeto idProjeto listaProjetos
    let membroNoProjeto = case projeto of
                            Just p -> membroEstaNoProjeto idMembroResponsavel p
                            Nothing -> False
    (if membroNoProjeto then (do
            editAtivDoUsuario "Database/usuarios.json" idMembroResponsavel [idAtividade]
            putStrLn $ ".---------------------------------------------------------." ++ "\n"
                     ++"|             Atividade atribuída com sucesso!            |" ++ "\n"
                     ++".---------------------------------------------------------." ++ "\n") else (do
            putStrLn $ ".---------------------------------------------------------." ++ "\n"
                     ++"|              Membro não está no projeto                 |" ++ "\n"
                     ++".---------------------------------------------------------." ++ "\n"))


-- -- Visualiza atividades cadastradas no sistema
-- bancoDeAtividades :: IO ()
-- bancoDeAtividades = do
--   putStrLn "Implementação em andamento."