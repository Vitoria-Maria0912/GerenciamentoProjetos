module Haskell.Menus.MenuGerente where
import Haskell.Util.ClearScreen
import Data.Char (toLower)


-- menuGerente
imprimeMenu :: IO()
imprimeMenu = do
    clearScreen
    putStrLn $
    "Selecione uma opção:" 
     ++ "1. Criar atividade\n" 
     ++ "2. Deletar atividade\n"
     ++ "3. Visualizar membros do projeto\n"
     ++ "4. Remover membro do projeto\n"
     ++ "5. Atribuir membro a uma atividade\n"
     ++ "6. Enviar mensagem a um membro\n"                                                    
     ++ "S. Sair do Sistema"                                                               


menuGerente :: IO()
menuGerente = do
    imprimeMenu
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 
        "1" -> criaAtividade
        "2" -> removeAtividade
        "3" -> membrosProjeto
        "4" -> removeMembroProjeto
        "5" -> atribuirMembro
        "6" -> mensagemMembro
        "s" -> sairDoSistema
        _   -> erroMenuGerente


-- caso o usuário digite o comando errado
erroMenuGerente :: IO()
erroMenuGerente = do
    putStrLn   "Entrada Inválida. Tente novamente!"
    menuGerente


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


-- Função para remover um projeto
removerProjeto :: IO()
removerProjeto = do
  putStrLn "Digite o ID do projeto:"
  idProjeto <- getLine
  removeProjeto idProjeto
  putStrLn "Projeto removido com sucesso!"


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


-- Enviar mensagem a um membro
mensagemMembro :: IO()
mensagemMembro = do
    putStrLn "Digite o nome do membro:"
    membro <- getLine
    putStrLn "Digite a mensagem que deseja enviar:"
    mensagem <- getLine
    -- invoca função para enviar mensagem a membro, em Mensagens.hs
    putStrLn "Mensagem enviada com sucesso!"

