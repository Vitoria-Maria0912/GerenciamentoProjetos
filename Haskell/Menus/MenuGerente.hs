module Menus.MenuGerente where
import Controllers.Atividades as Atividades
import Util.ClearScreen
import Data.Char (toLower)


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
        "r" -> removeAtividade
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
    putStrLn "Digite um título para sua atividade:"
    title <- getLine
    putStrLn "Descreva, brevemente, o que se deve realizar para concluir esta atividade."
    descricao <- getLine
    -- invoca função de criação, em Atividades.hs
    putStrLn "Tarefa criada com sucesso!"


-- remove atividade
removeAtividade :: IO()
removeAtividade = do
    putStrLn "Digite o nome da atividade que deseja remover:"
    title <- getLine
    -- invoca a função de remoção, em Atividades.hs
    putStrLn "Atividade removida com sucesso!"


-- Visualizar membros do projeto
membrosProjeto :: IO()
membrosProjeto = do
    putStrLn "Digite o nome do projeto:"
    projeto <- getLine
    -- invoca função para visualizar membros do projeto, em Projetos.hs
    putStrLn "Membros do projeto:"
    -- imprime os membros do projeto


-- Remover membro do projeto
removeMembroProjeto :: IO()
removeMembroProjeto = do
    putStrLn "Digite o nome do projeto:"
    projeto <- getLine
    putStrLn "Digite o nome do membro que deseja remover:"
    membro <- getLine
    -- invoca função para remover membro do projeto, em Projetos.hs
    putStrLn "Membro removido do projeto com sucesso!"


-- Atribuir membro a uma atividade
atribuirMembro :: IO()
atribuirMembro = do
    putStrLn "Digite o nome da atividade:"
    atividade <- getLine
    putStrLn "Digite o nome do membro que deseja atribuir:"
    membro <- getLine
    -- invoca função para atribuir membro a atividade, em Atividades.hs
    putStrLn "Membro atribuído à atividade com sucesso!"


-- Função para criar feedback
criarFeedback :: IO ()
criarFeedback = do
  -- Implementation logic for creating feedback
  putStrLn "Implementação em andamento."
  menuPrincipal