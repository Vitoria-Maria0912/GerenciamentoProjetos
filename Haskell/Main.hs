module Haskell.Main where
import Data.Char (toLower)
import qualified Haskell.FuncoesAuxiliares as FuncoesAuxiliares
import System.Exit (exitSuccess)


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn   "----------------------------------"
    putStrLn $ "Entrada Inválida. Tente novamente!"
    putStrLn   "----------------------------------\n"
    main


-- função que imprime o menu
menu :: IO()
menu = do
    putStrLn "Menu Principal:\n"
    putStrLn $ "C - criar perfil\n"
            ++ "D - deletar perfil\n"
            ++ "P - criar projeto\n"
            ++ "R - remover projeto\n"
            ++ "L - listar projetos\n"
            ++ "E - solicitar entrada em um projeto\n" -- poderia ser através de um código
            ++ "F - dar feedback de uma atividade realizada"
            ++ "M - abrir caixa de mensagens"
            ++ "B - visualizar banco de atividades"
            ++ "S - sair do sistema\n"
            ++ "\nEscolha uma opção: "


-- primeiro menu mostrado ao usuário
main :: IO()
main = do
    menu
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
        -- está como String, mas serão as funções
        "c" -> putStrLn "createProfile"
        "d" -> putStrLn "deleteProfile"
        "p" -> putStrLn "createProject"
        "r" -> putStrLn "removeProject"
        "l" -> putStrLn "viewProjectsInProgress"
        "e" -> putStrLn "requestEntry"
        "f" -> putStrLn "createFeedback"
        "m" -> putStrLn "chat"
        "b" -> putStrLn "activitiesBank"
        "s" -> exitSistem
        _   -> erroMenuPrincipal


-- sai do sistema
exitSistem :: IO()
exitSistem = putStrLn "Você saiu do sistema! Até a próxima!"


-- função para receber as entradas do usuário, referente a criação do perfil
createProfile :: IO()
createProfile = do
    putStrLn "Olá! Qual o seu nome?"
    name <- getLine
    -- criar uma função de amazenamento
    putStrLn $ "\nParabéns, " ++ name
            ++ ", você está cadastrado(a) no Sistema de Gerenciamento de Projetos!"
    main


-- função para receber as entradas do usuário, referente a exclusão de um perfil
deleteProfile :: IO()
deleteProfile = do
    putStrLn "Digite o nome do seu perfil: "
    name <- getLine
    putStrLn "Digite sua senha: "
    senha <- getLine
    -- tem fazer uma função para remover do sistema
    putStrLn $ name ++ ", seu perfil foi deletado com sucesso!"


-- função para receber as entradas referentes a criação de projeto
createProject :: IO()
createProject = do
    putStrLn "Vamos criar o seu projeto!"
    putStrLn ".........................."
    putStrLn "Qual o título do projeto?"
    nome <- getLine
    -- FUNÇÃO -> criar função para checar se já existe esse projeto com esse nome
    if  FuncoesAuxiliares.verificaNome nome then do
        putStrLn "Projeto já existente, por favor escolha outro nome."
        exitSistem -- coloquei para sair, para conseguir compilar e ficar mais simples
    else do
        putStrLn "\nDescreva, brevemente, seu projeto!"
        descricao <- getLine
        putStrLn "Projeto criado!"


-- recebe as entradas referentes a remoção de um projeto
removeProject :: IO()
removeProject = do
    putStrLn "Digite o nome do projeto que deseja deletar:"
    nome <- getLine
    -- checar se o projeto existe
    let naoExiste = False -- <<<< é só para compilar 
    -- este `naoExiste` poderia ser a função usada anteriormente
    -- em `createProject` "Project.Utils.verificaNome nome"
    if naoExiste
        then putStrLn "Projeto inexistente!"
    else do
    -- tem que ter uma verificação para saber se o usuário é gerente
        let gerente = False  -- <<<< é só para compilar 
        if gerente
            then putStrLn "Projeto removido com sucesso."
        else
            putStrLn "Você não pode executar essa ação. Você precisa ser gerente para realizá-la!"

