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
            ++ "|           B - Menu do banco de atividades                |" ++ "\n"
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
                menuRestritoProjeto
        "p" -> deletarProjeto
        "g" -> gerenciarMembros
        "b" -> menuBancoDeAtividades --bancoDeAtividades 
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

    --bancoDeAtividades 
    -- a visualização do banco deveria ter uma chamada propria

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

                                        menuRestritoProjeto
                else do
                        clearScreen
                        putStrLn $ ".----------------------------------------------------------." ++ "\n"
                                ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                                ++ ".----------------------------------------------------------." ++ "\n"
                        menuRestritoProjeto

-- Remove uma atividade de um projeto
deletaAtividade :: IO()
deletaAtividade = do

    --bancoDeAtividades

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                    Deletar atividade:                      " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID do projeto que deseja remover uma atividade: "
    idProjeto <- readLn :: IO Int

    -- CHECAR SE O PROJETO EXISTE
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
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|          Atividade inexistente! Tente novamente.         |" ++ "\n"
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
                "m" -> visualizarMembros
                "a" -> atribuirMembro
                "n" -> adicionaNovoMembro
                "r" -> removeMembroProjeto
                "v" -> menuRestritoProjeto
                _   -> erroMenuGerente
        Nothing -> do
            clearScreen
            putStrLn $ ".----------------------------------------------------------." ++ "\n"
                    ++ "|              ID inválido, tente novamente.               |" ++ "\n"
                    ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuRestrito 

-- | Exibe os membros de um projeto
visualizarMembros:: IO()
visualizarMembros = do
        
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                Visualizar membros do projeto:              " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID do projeto:"
    idProjeto <- readLn :: IO Int

    let projetosDoSistema = getTodosProjetos "Database/projetos.json"
    let projetoNoSistema = getProjeto idProjeto projetosDoSistema

    case projetoNoSistema of
        Just projeto -> do
                clearScreen
                let usuariosDoSistema = getUsuarios "Database/usuarios.json"
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "              Estes são os membros do projeto:              " ++ "\n"
                mapM_ imprimeMembrosDoProjeto usuariosDoSistema
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                
        Nothing -> do
                clearScreen
                putStrLn $ ".----------------------------------------------------------." ++ "\n"
                        ++ "|           Projeto inexistente! Tente novamente!          |" ++ "\n"
                        ++ ".----------------------------------------------------------." ++ "\n"
    retornoMenuRestrito  

-- | Adiciona um novo membro a um projeto
adicionaNovoMembro :: IO ()
adicionaNovoMembro = do

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "                   Adicionar novo membro:                   " ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  putStrLn "Digite o ID do projeto:"
  idProjeto <- readLn :: IO Int

  -- COLOCAR VERIFICACAO SE ESSE PROJETO ESTÁ NA LISTA DE PROJETOS
  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "Segue a lista de usuários disponíveis no sistema para adição no projeto: \n"
  mapM_ imprimirUsuario (getUsuarios "Database/usuarios.json")

  putStrLn $ ".---------------------------------------------------------." ++ "\n"
           ++"|          Digite o ID do membro que deseja adicionar:    |" ++ "\n"
           ++".---------------------------------------------------------." ++ "\n"
  idUsuario <- readLn :: IO Int

  let listaProjetos = getTodosProjetos "Database/projetos.json"
  let projeto = getProjeto idProjeto listaProjetos

  let membroNoProjeto = case projeto of
                            Just p -> membroEstaNoProjeto idUsuario p && idUsuario /= idGerente p
                            Nothing -> False

  if membroNoProjeto then do
          putStrLn $ ".---------------------------------------------------------." ++ "\n"
                  ++ "|             Membro já está no projeto!                  |" ++ "\n"
                  ++ ".---------------------------------------------------------." ++ "\n"
  else do
           editMembrosDoProjeto "Database/projetos.json" idProjeto idUsuario True
           putStrLn $ ".---------------------------------------------------------." ++ "\n"
                    ++"|             Membro adicionado com sucesso!              |" ++ "\n"
                    ++".---------------------------------------------------------." ++ "\n"
  retornoMenuRestrito

-- | Remover membro do projeto
removeMembroProjeto :: IO ()
removeMembroProjeto = do
  
  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "                  Remover membro do projeto:                " ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  putStrLn "Digite o ID do projeto:"
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
    putStrLn $  ".---------------------------------------------------------." ++ "\n"
             ++ "|             Membro removido do projeto!                 |" ++ "\n"
             ++ ".---------------------------------------------------------." ++ "\n"
  else do
    putStrLn $  ".---------------------------------------------------------." ++ "\n"
             ++ "|             Membro não está no projeto!                 |" ++ "\n"
             ++ ".---------------------------------------------------------." ++ "\n"
  retornoMenuRestrito

-- | Atribuir membro a uma atividade
atribuirMembro :: IO()
atribuirMembro = do

        -- FALTAM CHECAGENS
  
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "             Atribuir uma atividade a um membro:            " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn "Digite o ID da atividade:"
    idAtividade <- readLn :: IO Int
    putStrLn "Digite o ID do projeto que a atividade pertence/pertencerá:"
    idProjeto <- readLn :: IO Int
    putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
    idMembroResponsavel <- readLn :: IO Int


    let ativFilePath = "Database/bancoDeAtividades.json"


    let atividadeNoSistema = getAtividade idAtividade (getTodasAtividades ativFilePath)

    case atividadeNoSistema of
        Just _ -> do
                if temIdProjeto idAtividade (getTodasAtividades ativFilePath) == True then do 
                         let listaProjetos = getTodosProjetos "Database/projetos.json"
                         let projeto = getProjeto idProjeto listaProjetos
                         let membroNoProjeto = case projeto of
                                        Just p -> membroEstaNoProjeto idMembroResponsavel p
                                        Nothing -> False
                         if membroNoProjeto then do
                                editAtivDoUsuario "Database/usuarios.json" idMembroResponsavel [idAtividade]
                                editMembroResp ativFilePath idAtividade idMembroResponsavel True
                                putStrLn $ ".---------------------------------------------------------." ++ "\n"
                                         ++"|             Atividade atribuída com sucesso!            |" ++ "\n"
                                         ++".---------------------------------------------------------." ++ "\n"
                         else do
                                putStrLn $ ".---------------------------------------------------------." ++ "\n"
                                         ++"|              Membro não está no projeto                 |" ++ "\n"
                                         ++".---------------------------------------------------------." ++ "\n"
                else do
                        let listaProjetos = getTodosProjetos "Database/projetos.json"
                        let projeto = getProjeto idProjeto listaProjetos
                        editIdProj ativFilePath idAtividade idProjeto True
                        putStrLn $ ".---------------------------------------------------------." ++ "\n"
                                 ++"|           Projeto atrelado a atividade!                 |" ++ "\n"
                                 ++".---------------------------------------------------------." ++ "\n"
                        let membroNoProjeto = case projeto of
                                        Just p -> membroEstaNoProjeto idMembroResponsavel p
                                        Nothing -> False
                        if membroNoProjeto then do
                                editAtivDoUsuario "Database/usuarios.json" idMembroResponsavel [idAtividade]
                                putStrLn $ ".---------------------------------------------------------." ++ "\n"
                                         ++"|             Atividade atribuída com sucesso!            |" ++ "\n"
                                         ++".---------------------------------------------------------." ++ "\n"
                        else do
                                clearScreen
                                putStrLn $ ".---------------------------------------------------------." ++ "\n"
                                         ++"|              Membro não está no projeto                 |" ++ "\n"
                                         ++".---------------------------------------------------------." ++ "\n"
        Nothing -> do
                putStrLn $ ".---------------------------------------------------------." ++ "\n"
                         ++"|                 Atividade inexistente!                  |" ++ "\n"
                         ++".---------------------------------------------------------." ++ "\n"
      




-- | Visualiza atividades cadastradas no sistema
bancoDeAtividades :: IO ()
bancoDeAtividades = do

    let atividadesCadastradas = (getTodasAtividades "Database/bancoDeAtividades.json")
    clearScreen
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                  Banco de Atividades:                      " ++ "\n"
    mapM_ imprimirAtividade atividadesCadastradas
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
    menuBancoDeAtividades

-- Função para o menu de banco de atividades no menu do gerente
menuBancoDeAtividades :: IO ()
menuBancoDeAtividades = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "                Menu Banco de Atividades                    " ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                                                           |" ++ "\n"
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
