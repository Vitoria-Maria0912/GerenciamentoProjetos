module Menus.MenuGerente where

import System.Random
import Controllers.Atividades as Atividades
import Menus.MenuPublico as MenuPublico
import Database.Database
import Util.ClearScreen
import Data.Char (toLower)


-- Exibe erro e retorna ao menu
erroMenuGerente :: IO()
erroMenuGerente = do
    putStrLn $ "------------------------------------------------------------\n"
            ++ "|            Entrada Inválida. Tente novamente!            |\n"
            ++ "------------------------------------------------------------\n"
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
            ++ "|           C - Criar uma atividade                        |" ++ "\n"
            ++ "|           G - Gerenciar membros do projeto               |" ++ "\n"
            ++ "|           R - Remover uma atividade                      |" ++ "\n"
            ++ "|           D - Atribuir atividade a um membro             |" ++ "\n"
            ++ "|           J - Remover membro do projeto                  |" ++ "\n"
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

        "p" -> removerProjeto
        "c" -> criaAtividade
        "g" -> gerenciarMembros
        "r" -> deletarAtividade 
        "d" -> atribuirMembro
        "j" -> removeMembroProjeto
        "i" -> MenuPublico.comecarAtividade
        "f" -> MenuPublico.finalizarAtividade
        "v" -> MenuPublico.visualizarAtividades
        "a" -> MenuPublico.statusAtividade
        "o" -> MenuPublico.criarFeedback
        "s" -> MenuPublico.sairDoSistema
        _   -> erroMenuGerente


-- Remove um projeto do sistema
removerProjeto :: IO()
removerProjeto = do

    putStrLn "Digite seu ID:"
    idUsuario <- getLine
    putStrLn "Digite o ID do projeto que deseja excluir:"
    idProjeto <- getLine

    -- let projeto = Database.getProjeto idProjeto
    -- let gerente = Util.ehGerente idUsuario projeto

    -- if gerente then do Database.removeProjeto idProjeto putStrLn "Projeto removido com sucesso!"
    -- else putStrLn "Você não tem permissão para deletar esse projeto!"

    putStrLn ""

-- Cria uma atividade em um projeto
criaAtividade :: IO()
criaAtividade = do
    putStrLn "Digite o ID do projeto que deseja adicionar uma atividade:"
    idProjeto <- getLine

    putStrLn "Digite um título para sua atividade:"
    titulo <- getLine

    putStrLn "Descreva, brevemente, o que se deve realizar para concluir esta atividade."
    descricao <- getLine

    idAtividade <- randomRIO (10000, 99999 :: Int)

    -- tem que checar se já existe
    let novaAtividade = Atividades.criarAtividade titulo descricao "Não atribuída" (show(idAtividade)) idProjeto

    -- Atividades.adicionaAtividade novaAtividade Projeto.getAtividades

    putStrLn "Tarefa criada com sucesso!"

-- Remove uma atividade de um projeto
deletarAtividade :: IO()
deletarAtividade = do
    putStrLn "Digite o ID da atividade que deseja remover:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine

    -- let projeto = getProjeto idProjeto
    -- let atividade = projeto.getAtividade idAtividade
    -- let atividadeExiste = Util.verificaIdAtividade projeto idAtividade

    -- if atividadeExiste then do atividades.removerAtividade idAtividade putStrLn "Atividade removida com sucesso!"
    -- else putStrLn "Atividade inexistente!"

    putStrLn ""

-- Visualizar membros do projeto
gerenciarMembros :: IO()
gerenciarMembros = do
    putStrLn "Digite o ID do projeto:"
    idProjeto <- getLine

    -- let projeto = getProjeto idProjeto
    
    -- invoca função para visualizar membros do projeto, em Projetos.hs
    putStrLn "Membros do projeto:"
    -- imprime os membros do projeto


-- Remover membro do projeto
removeMembroProjeto :: IO()
removeMembroProjeto = do
    putStrLn "Digite o ID do projeto:"
    idProjeto <- getLine
    putStrLn "Digite o ID do membro que deseja remover:"
    usuario <- getLine
    
    -- let projeto = getProjeto idProjeto

    -- projeto.removeMembroProjeto idProjeto usuario
    putStrLn "Membro removido do projeto com sucesso!"


-- Atribuir membro a uma atividade
atribuirMembro :: IO()
atribuirMembro = do
    putStrLn "Digite o ID da atividade:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine
    putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
    membroResponsavel <- getLine

    -- let projeto = Projeto.
    -- let atividade = getAtividade idAtividade
    -- Atividades.atribuirMembro atividade membroResponsavel

    -- TEM QUE ARMAZENÁ-LOS

    putStrLn "Membro atribuído à atividade com sucesso!"
