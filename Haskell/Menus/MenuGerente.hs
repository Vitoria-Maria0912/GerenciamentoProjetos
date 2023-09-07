module Menus.MenuGerente where
import Controllers.Atividades as Atividades
import Database.Database
import Util.ClearScreen
import Data.Char (toLower)
import Menus.MenuGeral (menuPrincipal)

erroMenuGerente :: IO()
erroMenuGerente = do
    putStrLn   "----------------------------------"
    putStrLn   "Entrada Inválida. Tente novamente!"
    putStrLn   "----------------------------------\n"
    menuRestritoAtividades


menuRestritoAtividades :: IO()
menuRestritoAtividades = do 

    clearScreen
    
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                   Menu Projeto                           |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                Selecione uma opção:                      |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|              C - Criar uma atividade                     |" ++ "\n"
            ++ "|              G - Gerenciar membros do projeto            |" ++ "\n"
            ++ "|              R - Remover uma atividade                   |" ++ "\n"
            ++ "|              I - Iniciar uma atividade                   |" ++ "\n"
            ++ "|              F - Finalizar uma atividade                 |" ++ "\n"
            ++ "|              V - Visualizar atividades do projeto        |" ++ "\n"
            ++ "|              A - Visualizar status de uma atividade      |" ++ "\n"
            ++ "|              O - Dar feedback em uma atividade           |" ++ "\n"
            ++ "|              D - Atribuir atividade a um membro          |" ++ "\n"
            ++ "|              J - Remover membro do projeto               |" ++ "\n"
            ++ "|              M - Voltar ao menu principal                |" ++ "\n"
            ++ "|              S - Sair do sistema                         |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "c" -> criarAtividade
        "g" -> gerenciarMembros
        "r" -> deletarAtividade
        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criarFeedback
        "d" -> atribuirMembro
        "j" -> removeMembroProjeto
        "m" -> menuPrincipal
        "s" -> sairDoSistema
        _   -> erroMenuGerente


-- sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"


-- cria atividade
criaAtividade :: IO()
criaAtividade = do
    putStrLn "Digite o ID do projeto que deseja adicionar uma atividade:"
    idProjeto <- getLine

    putStrLn "Digite um título para sua atividade:"
    titulo <- getLine

    putStrLn "Descreva, brevemente, o que se deve realizar para concluir esta atividade."
    descricao <- getLine

    idAtividade <- randomRIO (1000, 9999 :: Int)

    -- tem que checar se já existe
    Atividades.criarAtividade titulo descricao "Não atribuída" (show(idAtividade)) idProjeto "Não atribuído"

    putStrLn "Tarefa criada com sucesso!"


-- remove atividade
deletarAtividade :: IO()
deletarAtividade = do
    putStrLn "Digite o ID da atividade que deseja remover:"
    idAtividade <- getLine

    -- tem que checar se existe
    -- Atividades.removeAtividade idAtividade
    putStrLn "Atividade removida com sucesso!"


statusAtividade :: IO()
statusAtividade = do

    -- acessaria pelo nome ou pelo ID?
    putStrLn "Digite o id da atividade que deseja visualizar o status:"
    idAtividade <- getLine

    -- tem que passar atividade, mas acho que seria melhor pelo ID
    putStrLn "Atividades.mostraStatus titulo"

visualizarAtividades :: IO()
visualizarAtividades = do 

    -- como acessar as atividades do projeto sem precisar digitar o título dele?
    -- seria bom uma maneira de 'exibir o arquivo'
    -- mas será possível ver apenas o nome ou, por exemplo, a quantidade de membros
    -- em cada atividade?
   
    putStrLn "Projeto.exibeAtividades projeto"

-- Visualizar membros do projeto
gerenciarMembros :: IO()
gerenciarMembros = do
    putStrLn "Digite o nome do projeto:"
    projeto <- getLine
    -- invoca função para visualizar membros do projeto, em Projetos.hs
    putStrLn "Membros do projeto:"
    -- imprime os membros do projeto


-- Remover membro do projeto
removeMembroProjeto :: IO()
removeMembroProjeto = do
    putStrLn "Digite o ID do projeto:"
    idProjeto <- getLine
    putStrLn "Digite o ID do membro que deseja remover:"
    membroResponsavel <- getLine
    
    -- Projeto.removeMembroProjeto idProjeto membroResponsavel
    putStrLn "Membro removido do projeto com sucesso!"


-- Atribuir membro a uma atividade
atribuirMembro :: IO()
atribuirMembro = do
    putStrLn "Digite o ID da atividade:"
    idAtividade <- getLine
    putStrLn "Digite o ID do membro que deseja atribuir à atividade:"
    membroResponsavel <- getLine
    
    -- Atividades.atribuirMembro idAtividade membroResponsavel
    putStrLn "Membro atribuído à atividade com sucesso!"


-- Função para criar feedback
criarFeedback :: IO ()
criarFeedback = do
  -- Implementation logic for creating feedback
  putStrLn "Implementação em andamento."
  menuRestritoAtividades