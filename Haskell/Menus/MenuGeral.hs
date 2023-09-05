module Haskell.Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import System.Random
import Data.Char (toLower)
import Haskell.Util.ClearScreen
import Haskell.Controllers.Usuario
import Haskell.Controllers.Projeto
import Haskell.Menus.MenuGerente (menuGerente)

-- menuGeral
menuGeral :: IO()
menuGeral = do
    menuPrincipal
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
        "g" -> menuGerente
        "c" -> cadastro
        "d" -> deletarUsuario
        "p" -> cadastrarProjeto
        "r" -> removerProjeto
        "l" -> visualizarProjetosPendentes
        "e" -> solicitarEntrada
        "f" -> criarFeedback
        "m" -> chat
        "b" -> bancoDeAtividades
        "s" -> sairDoSistema
        _   -> erroMenuPrincipal


menuPrincipal :: IO ()
menuPrincipal = do
  clearScreen
  putStrLn ".--------------------------------------------------."
  putStrLn "|oooo     oooo ooooooooooo oooo   oooo ooooo  oooo |"
  putStrLn "| 8888o   888   888    88   8888o  88   888    88  |"
  putStrLn "| 88 888o8 88   888ooo8     88 888o88   888    88  |"
  putStrLn "| 88  888  88   888    oo   88   8888   888    88  |"
  putStrLn "|o88o  8  o88o o888ooo8888 o88o    88    888oo88   |"
  putStrLn "|                                                  |"
  putStrLn "|Selecione uma opção:                              |"
  putStrLn "|                                                  |"
  putStrLn "|G. Menu Gerenciamento de Projetos e atividades    |"
  putStrLn "|C. Cadastrar novo usuário                         |"
  putStrLn "|D. Deletar Perfil                                 |"
  putStrLn "|P. Criar Projeto                                  |"
  putStrLn "|R. Remover Projeto                                |"
  putStrLn "|L. Listar Projetos em andamento                   |"
  putStrLn "|E. Solicitar Entrada em projeto                   |"
  putStrLn "|F. Dar feedback em uma atividade                  |"
  putStrLn "|M. Caixa de Mensagens                             |"
  putStrLn "|B. Visualizar Banco de atividades                 |"
  putStrLn "|S. Sair do Sistema                                |"
  putStrLn ".--------------------------------------------------."


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn   "----------------------------------"
    putStrLn   "Entrada Inválida. Tente novamente!"
    putStrLn   "----------------------------------\n"
    menuGeral


-- sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"



-- Função para cadastrar um novo usuário
cadastro :: IO ()
cadastro = do
  clearScreen
  putStrLn "Menu>Cadastro"
  putStrLn "\n"
  putStrLn "Digite seu nome: "
  nome <- getLine
  putStrLn "Digite sua senha: "
  senha <- getLine

  numAleatorio <- randomRIO (1000, 9999 :: Int)
-- cria um numero aleatorio para o id do usuario
  idUsuario <- numAleatorioUsuario
-- checa se já existe um usuario com esse Id (visto que numAleatorio é na verdade pseudorrandômicos
  if (Usuario.verificaIdExistente (read idUsuario) usuarios == False) then do
  criaUsuario idUsuario nome senha
-- ver se o id que vai ser passado aqui vai ser o mesmo que foi gerado antes
  putStrLn "Usuário cadastrado com sucesso. Seu id é:" (show idUsuario) -- nao sei se pode ficar assim
  menuPrincipal
  else do
    putStrLn "O id já existe na base de dados."
    putStrLn ""
    cadastro
    

-- Função para deletar um usuário
deletarUsuario :: IO()
deletarUsuario = do
  clearScreen
  putStrLn "Menu>Deletar Usuário"
  putStrLn "\n"                          
  putStrLn "Digite o id do usuário"
  idUsuario <- getLine
  if (Usuario.verificaIdExistente (read idUsuario) usuarios == True) then do
  removeUsuario idUsuario
  putStrLn "Usuário deletado com sucesso."
  menuPrincipal
  else do
    putStrLn "O id não existe na base de dados."
    putStrLn ""
    deletarUsuario
  
  

-- Função para criar um projeto
cadastrarProjeto :: IO()
cadastrarProjeto = do
  clearScreen
  putStrLn "Menu>Cadastrar Projeto"
  putStrLn "|------------------------------------------------------------------------------------|"
  putStrLn "|  #####                            ######                                           |"
  putStrLn "| #     # #####  #   ##   #####     #     # #####   ####       # ###### #####  ####  |"
  putStrLn "| #       #    # #  #  #  #    #    #     # #    # #    #      # #        #   #    # |"
  putStrLn "| #       #    # # #    # #    #    ######  #    # #    #      # #####    #   #    # |"
  putStrLn "| #       #####  # ###### #####     #       #####  #    #      # #        #   #    # |"
  putStrLn "| #     # #   #  # #    # #   #     #       #   #  #    # #    # #        #   #    # |"
  putStrLn "|  #####  #    # # #    # #    #    #       #    #  ####   ####  ######   #    ####  |"
  putStrLn "|------------------------------------------------------------------------------------|"
  putStrLn "\n"
  putStrLn "Digite seu nome:"
  nomeUsuario <- getLine
  putStrLn "Digite o nome do projeto:"
  nomeProjeto <- getLine
  putStrLn "Digite a descrição do seu projeto:"
  descricao <- getLine
  putStrLn "Digite um ID para seu projeto:"
  id <- getLine
  criaProjeto id nomeProjeto descricao nomeUsuario



-- Função para remover um projeto
removerProjeto :: IO()
removerProjeto = do
  clearScreen
  putStrLn "Menu>Deletar Projeto"
  putStrLn "|---------------------------------------------------|"
  putStrLn "| ######                                            |"
  putStrLn "| #     # ###### #      ###### #####   ##   #####   |"
  putStrLn "| #     # #      #      #        #    #  #  #    #  |"
  putStrLn "| #     # ###### #      #####    #   #    # #    #  |"
  putStrLn "| #     # #      #      #        #   ###### #####   |" 
  putStrLn "| #     # #      #      #        #   #    # #    #  |"
  putStrLn "| ######  ###### ###### ######   #   #    # #    #  |"
  putStrLn "|---------------------------------------------------|"
  putStrLn "\n"
  putStrLn "Digite o nome do projeto:"
  nomeProjeto <- getLine
  putStrLn "Digite o ID do projeto:"
  idProjeto <- getLine
  removeProjeto nomeProjeto idProjeto



-- Função para visualizar projetos pendentes
visualizarProjetosPendentes :: IO ()
visualizarProjetosPendentes = do
  -- Implementation logic for viewing pending projects
  putStrLn "Implementação em andamento."
  menuPrincipal



-- Função para solicitar entrada em um projeto
solicitarEntrada :: IO ()
solicitarEntrada = do
  -- Implementation logic for requesting entry into a project
  putStrLn "Implementação em andamento."
  menuPrincipal



-- Função para criar feedback
criarFeedback :: IO ()
criarFeedback = do
  -- Implementation logic for creating feedback
  putStrLn "Implementação em andamento."
  menuPrincipal



-- Função para entrar no chat
chat :: IO ()
chat = do
  -- Implementation logic for entering the chat
  putStrLn "Implementação em andamento."
  menuPrincipal



-- Função para visualizar banco de atividades
bancoDeAtividades :: IO ()
bancoDeAtividades = do
  -- Implementation logic for viewing activity bank
  putStrLn "Implementação em andamento."
  menuPrincipal
