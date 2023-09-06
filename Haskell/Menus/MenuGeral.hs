module Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import Data.Char (toLower)
import Util.ClearScreen
import Controllers.Usuario
import Controllers.Projeto
-- import Menus.MenuGerente


menuPrincipal :: IO ()
menuPrincipal = do

  clearScreen

  putStrLn $ ".--------------------------------------------------."
          ++ "|Selecione uma opção:                              |"
          ++ "|                                                  |"
          ++ "|G. Menu Gerenciamento de Projetos                 |"
          ++ "|C. Cadastrar novo usuário                         |"
          ++ "|D. Deletar Perfil                                 |"
          ++ "|P. Criar Projeto                                  |"
          ++ "|R. Remover Projeto                                |"
          ++ "|L. Listar Projetos em andamento                   |"
          ++ "|E. Solicitar Entrada em projeto                   |"
          ++ "|F. Dar feedback em uma atividade                  |"
          ++ "|M. Caixa de Mensagens                             |"
          ++ "|B. Visualizar Banco de atividades                 |"
          ++ "|S. Sair do Sistema                                |"
          ++ ".--------------------------------------------------."

  option <- getLine
  let lowerOption = map toLower option
  case lowerOption of
        -- "g" -> menuProjeto
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


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn   "----------------------------------"
    putStrLn   "Entrada Inválida. Tente novamente!"
    putStrLn   "----------------------------------\n"
    menuPrincipal


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
  criaUsuario nome senha
  menuPrincipal



-- Função para deletar um usuário
deletarUsuario :: IO()
deletarUsuario = do
  clearScreen
  putStrLn "Menu>Deletar Usuário"
  putStrLn "\n"                          
  putStrLn "Digite nome do usuário:"
  nome <- getLine
  putStrLn "Digite sua senha:"
  senha <- getLine
  -- Tem que ter uma função para verificar se a senha bate com o nome do usuário
  removeUsuario nome
  menuPrincipal



-- Função para criar um projeto
cadastrarProjeto :: IO()
cadastrarProjeto = do
  clearScreen
  putStrLn "Menu>Cadastrar Projeto"
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