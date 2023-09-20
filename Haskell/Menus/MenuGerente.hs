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
            ++ "|           L - Listar projetos cadastrados                |" ++ "\n"
            ++ "|           P - Remover projeto                            |" ++ "\n"
            ++ "|           G - Gerenciar membros do projeto               |" ++ "\n"
            ++ "|           B - Visualizar banco de atividades             |" ++ "\n"
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

        "l" -> do
                visualizarProjetos
                retornoMenuRestrito
        "p" -> deletarProjeto
        "g" -> gerenciarMembros
        "b" -> menuBancoDeAtividades 
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criaFeedback
        "s" -> sairDoSistema
        _   -> erroMenuGerente

-- | Função do menu para remover um projeto pelo ID
deletarProjeto :: IO ()
deletarProjeto = do

  let projectFilePath = "Database/projetos.json"

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "                    Remover Projeto:                        " ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  putStrLn "Digite seu ID: "
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

                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
  else do
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|             ID inexistente! Tente novamente!             |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
  retornoMenuRestrito

-- | Cria uma atividade em um projeto
criaAtividade :: IO()
criaAtividade = do  

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                      Criar atividade:                      " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn $ "Digite o ID do projeto que deseja adicionar uma atividade: " ++ "\n"
             ++ "Caso você só deseje adicionar ao banco de atividades, digite 0"

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

            let atividadeNoSistema = getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json")

            case atividadeNoSistema of
                Just _ ->  do
                    clearScreen
                    putStrLn $ ".----------------------------------------------------------." ++ "\n"
                            ++ "|            Falha no cadastro! Tente novamente!           |" ++ "\n"
                            ++ ".----------------------------------------------------------." ++ "\n"

                Nothing -> do
                        editAtivDoProjeto "Database/projetos.json" idProjeto idAtividade True
                        criarAtividade "Database/bancoDeAtividades.json" titulo descricao (Just idProjeto) idAtividade Nothing []
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "  Atividade criada com sucesso! O ID da atividade é: " ++ show idAtividade ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"

        Nothing -> do
                if idProjeto == 0 then do
                        putStrLn "\nDigite um título para sua atividade: "
                        titulo <- getLine

                        putStrLn "\nDescreva, brevemente, o que se deve realizar para concluir esta atividade."
                        descricao <- getLine

                        idAtividade <- randomRIO (10010, 99999 :: Int)

                        let atividadeNoSistema = getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json")

                        case atividadeNoSistema of
                                Just _ ->  do
                                     clearScreen
                                     putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "|            Falha no cadastro! Tente novamente!           |" ++ "\n"
                                                 ++ ".----------------------------------------------------------." ++ "\n"
                                     criaAtividade
                                Nothing -> do
                                        criarAtividade "Database/bancoDeAtividades.json" titulo descricao Nothing idAtividade Nothing []
                                        clearScreen
                                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                                ++ "  Atividade criada com sucesso! O ID da atividade é: " ++ show idAtividade ++ "\n"
                                                ++ ".----------------------------------------------------------." ++ "\n"
                else do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuRestrito

-- Remove uma atividade de um projeto
deletaAtividade :: IO()
deletaAtividade = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                    Deletar atividade:                      " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID do projeto que deseja remover uma atividade: "
    idProjeto <- readLn :: IO Int

    let projetoNoSistema = getProjeto idProjeto (getTodosProjetos "Database/projetos.json")

    if isJust(projetoNoSistema) then do
        putStrLn "\nDigite o ID da atividade que deseja remover:"
        idAtividade <- readLn :: IO Int

        let atividadesNoSistema = getAtividade idAtividade (getTodasAtividades "Database/bancoDeAtividades.json")

        case atividadesNoSistema of
                Just atividade ->  do
                        editAtivDoProjeto "Database/projetos.json" idProjeto idAtividade False
                        editIdProj "Database/bancoDeAtividades.json" idAtividade idProjeto  False
                        editMembroResp "Database/bancoDeAtividades.json" idAtividade 0 False
                        editStatus "Database/bancoDeAtividades.json" idAtividade "Não atribuída!"
                        editFeedback "Database/bancoDeAtividades.json" idAtividade "" False

                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|              Atividade removida com sucesso!             |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"

                Nothing -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|          Atividade inexistente! Tente novamente.         |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"

    else do
        clearScreen
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "|             Falha no cadastro! Tente novamente!          |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
        
    retornoMenuRestrito

-- | Visualizar todos os membros do projeto
gerenciarMembros :: IO ()
gerenciarMembros = do

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                 Gerenciamento de membros:                  " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID do projeto:"
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos "Database/projetos.json"
    let projetoNoSistema = getProjeto idProjeto projetosDoSistema

    case projetoNoSistema of
        Just projeto -> do
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                     ++ "|                O que deseja fazer agora?                 |" ++ "\n"
                     ++ "|                                                          |" ++ "\n"
                     ++ "|                  Selecione uma opção:                    |" ++ "\n"
                     ++ "|                                                          |" ++ "\n"
                     ++ "|           M - Visualizar membros do projeto              |" ++ "\n"
                     ++ "|           A - Atribuir atividade a um membro             |" ++ "\n"
                     ++ "|           N - Adicionar membro ao projeto                |" ++ "\n"
                     ++ "|           R - Remover membro do projeto                  |" ++ "\n"
                     ++ "|           V - Voltar ao menu do projeto                  |" ++ "\n"
                     ++ ".----------------------------------------------------------." ++ "\n"

            option <- getLine
            let lowerOption = map toLower option
            case lowerOption of
                "m" -> visualizarMembros projeto
                -- "a" -> atribuirMembro projeto
                "n" -> adicionaNovoMembro projeto
                "r" -> removeMembroProjeto projeto
                "v" -> menuRestritoProjeto
                _   -> erroMenuGerente
        Nothing -> do
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "|              ID inválido, tente novamente.               |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"
            retornoMenuRestrito

-- | Exibe os membros de um projeto
visualizarMembros:: Projeto -> IO()
visualizarMembros projeto = do

        clearScreen
        let membrosDoProjeto = getMembrosDoProjeto (getIdsMembrosDoProjeto projeto) (getUsuarios "Database/usuarios.json")
        
        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                ++ "         Estes são os membros do projeto: (ID " ++ show(idProjeto projeto) ++ ")\n"
        mapM_ imprimeMembrosDoProjeto membrosDoProjeto
        putStrLn $ ".-----------------------------------------------------------." ++ "\n"
        retornoMenuRestrito  

-- | Adiciona um novo membro a um projeto
adicionaNovoMembro :: Projeto -> IO()
adicionaNovoMembro projeto = do

  clearScreen
  putStrLn $ ".---------------------------------------------------------." ++ "\n"
          ++ "                  Adicionar novo membro:                   " ++ "\n"
          ++ "                                                           " ++ "\n"
          ++ " Usuários disponíveis no sistema para adição no projeto: \n"
  mapM_ imprimirUsuario (getUsuarios "Database/usuarios.json")

  putStrLn $ "\n        Digite o ID do membro que deseja adicionar:        " ++ "\n"
           ++  ".---------------------------------------------------------." ++ "\n"
  idUsuario <- readLn :: IO Int

  if (membroEstaNoProjeto idUsuario projeto) then do
        clearScreen
        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                ++ "|             Membro já está no projeto!                  |" ++ "\n"
                ++ ".---------------------------------------------------------." ++ "\n"
  else if (idUsuario == (idGerente projeto)) then do
        clearScreen
        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                ++ "|           O ID pertence ao gerente do projeto!          |" ++ "\n"
                ++ ".---------------------------------------------------------." ++ "\n"
  else do
        clearScreen
        editMembrosDoProjeto "Database/projetos.json" (idProjeto projeto) idUsuario True
        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                ++"|             Membro adicionado com sucesso!              |" ++ "\n"
                ++".---------------------------------------------------------." ++ "\n"
  retornoMenuRestrito

-- | Remover membro do projeto
removeMembroProjeto :: Projeto -> IO()
removeMembroProjeto projeto = do
  
  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "                  Remover membro do projeto:                " ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  putStrLn "Digite o ID do membro que deseja remover:"
  idUsuario <- readLn :: IO Int

  if (membroEstaNoProjeto idUsuario projeto) then do
        clearScreen
        editMembrosDoProjeto "Database/projetos.json" (idProjeto projeto) idUsuario False
        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                ++ "|             Membro removido do projeto!                 |" ++ "\n"
                ++ ".---------------------------------------------------------." ++ "\n"
  else if (idUsuario == (idGerente projeto)) then do
        clearScreen
        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                ++ "|           O ID pertence ao gerente do projeto!          |" ++ "\n"
                ++ ".---------------------------------------------------------." ++ "\n"
  else do
        putStrLn $  ".---------------------------------------------------------." ++ "\n"
                ++ "|             Usuário não está no projeto!                 |" ++ "\n"
                ++ ".----------------------------------------------------------." ++ "\n"
  retornoMenuRestrito

-- | Atribuir membro a uma atividade
-- atribuirMembro :: Projeto -> IO()
-- atribuirMembro projeto = do

--     putStrLn $ ".----------------------------------------------------------." ++ "\n"
--             ++ "             Atribuir uma atividade a um membro:            " ++ "\n"
--             ++ ".----------------------------------------------------------." ++ "\n"

--     putStrLn "Digite o ID da atividade:"
--     idAtividade <- readLn :: IO Int

--     let ativFilePath = "Database/bancoDeAtividades.json"

--     let atividadeNoSistema = getAtividade idAtividade (getTodasAtividades ativFilePath)

--     case atividadeNoSistema of
--         Just _ -> do
--                 putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
--                 idMembroResponsavel <- readLn :: IO Int
--                 if temIdProjeto idAtividade (getTodasAtividades ativFilePath) == True then do 
                         
--                          if (membroEstaNoProjeto idMembroResponsavel projeto) then do
--                                 clearScreen
--                                 editAtivDoUsuario "Database/usuarios.json" idMembroResponsavel [idAtividade]
--                                 editMembroResp ativFilePath idAtividade idMembroResponsavel True
--                                 putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                                          ++"|             Atividade atribuída com sucesso!            |" ++ "\n"
--                                          ++".---------------------------------------------------------." ++ "\n"
--                          else do
--                                 clearScreen
--                                 putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                                          ++"|              Membro não está no projeto                 |" ++ "\n"
--                                          ++".---------------------------------------------------------." ++ "\n"
--                 else do
                        
--                         editIdProj ativFilePath idAtividade (idProjeto projeto) True
--                         putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                                  ++"|           Projeto atrelado a atividade!                 |" ++ "\n"
--                                  ++".---------------------------------------------------------." ++ "\n"
--                         if (membroEstaNoProjeto idMembroResponsavel projeto) then do
--                                 clearScreen
--                                 editAtivDoUsuario "Database/usuarios.json" idMembroResponsavel [idAtividade]
--                                 putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                                          ++"|             Atividade atribuída com sucesso!            |" ++ "\n"
--                                          ++".---------------------------------------------------------." ++ "\n"
--                         else do
--                                 clearScreen
--                                 putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                                          ++"|              Membro não está no projeto                 |" ++ "\n"
--                                          ++".---------------------------------------------------------." ++ "\n"
--         Nothing -> do
--                 clearScreen
--                 putStrLn $ ".---------------------------------------------------------." ++ "\n"
--                          ++"|                 Atividade inexistente!                  |" ++ "\n"
--                          ++".---------------------------------------------------------." ++ "\n"
      
--     retornoMenuRestrito


-- | Visualiza atividades cadastradas no sistema
bancoDeAtividades :: IO ()
bancoDeAtividades = do

    let atividadesCadastradas = (getTodasAtividades "Database/bancoDeAtividades.json")
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                  Banco de Atividades:                      " ++ "\n"
    mapM_ imprimirAtividade atividadesCadastradas
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
    

-- Função para o menu de banco de atividades no menu do gerente
menuBancoDeAtividades :: IO ()
menuBancoDeAtividades = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                Menu Banco de Atividades                    " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID da atividade:"
    idAtividade <- readLn :: IO Int

    let atividadesDoSistema = getTodasAtividades "Database/bancoDeAtividades.json"
    let  atividadeNoSistema = getAtividade idAtividade atividadesDoSistema

    case atividadeNoSistema of
        Just atividade -> do
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "|                Selecione uma opção:                       |" ++ "\n"
                    ++ "|                                                           |" ++ "\n"
                    ++ "|           C - Criar uma atividade                         |" ++ "\n"
                    ++ "|           R - Remover uma atividade                       |" ++ "\n"
                    ++ "|           L - Listar atividades cadastradas               |" ++ "\n"
                    ++ "|           A - Consultar uma atividade por ID              |" ++ "\n"
                    ++ "|           V - Voltar ao menu principal                    |" ++ "\n"
                    ++ "|           S - Sair do sistema                             |" ++ "\n"
                    ++ ".-----------------------------------------------------------." ++ "\n"
            option <- getLine
            let lowerOption = map toLower option
            case lowerOption of
                "l" -> bancoDeAtividades
                "c" -> criaAtividade
                "r" -> deletaAtividade
                "v" -> menuRestritoProjeto
                "s" -> sairDoSistema
                _   -> erroMenuGerente
        Nothing -> do
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "|              Atividade inválida, tente novamente.        |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuRestrito


-- | Retorna ao menu principal ou sai do sistema
retornoMenuRestrito :: IO()
retornoMenuRestrito = do
        putStrLn $ ".-----------------------------------------------------------." ++ "\n"
                ++ "| Você deseja voltar ao menu do projeto ou sair do sistema? |" ++ "\n"
                ++ "|                                                           |" ++ "\n"
                ++ "|                   M - Menu de projetos                    |" ++ "\n"
                ++ "|                   S - Sair do sistema                     |" ++ "\n"
                ++ ".-----------------------------------------------------------." ++ "\n"
        opcao <- getLine
        let lowerOption = map toLower opcao
        
        case lowerOption of
                "m" -> menuRestritoProjeto
                "s" -> sairDoSistema
                _   -> do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|            Entrada Inválida. Tente novamente!            |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n" 
                        retornoMenuRestrito
