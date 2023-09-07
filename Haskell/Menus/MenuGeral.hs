module Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import System.Random (Random(randomRIO))
import Data.Char (toLower)
import Util.ClearScreen
import Controllers.Usuario
import Controllers.Projeto
import Menus.MenuGerente (menuRestritoAtividades)


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn $ "----------------------------------"
          ++   "Entrada Inválida. Tente novamente!"
          ++   "----------------------------------\n"
    menuPrincipal


menuPrincipal :: IO ()
menuPrincipal = do

  clearScreen

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "|                      Menu Principal                      |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|                   Selecione uma opção:                   |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|             G - Menu de projetos                         |" ++ "\n"
          ++ "|             C - Cadastrar novo usuário                   |" ++ "\n"
          ++ "|             D - Deletar perfil                           |" ++ "\n"
          ++ "|             P - Criar projeto                            |" ++ "\n"
          ++ "|             R - Remover projeto                          |" ++ "\n"
          ++ "|             L - Listar projetos em andamento             |" ++ "\n"
          ++ "|             E - Solicitar entrada em projeto             |" ++ "\n"
          ++ "|             M - Caixa de mensagens                       |" ++ "\n"
          ++ "|             B - Visualizar banco de atividades           |" ++ "\n"
          ++ "|             S - Sair do sistema                          |" ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"

  option <- getLine
  let lowerOption = map toLower option
  case lowerOption of
      "g" -> menuProjetos
      "c" -> cadastrarUsuario
      "d" -> deletarUsuario
      "p" -> cadastrarProjeto
      "r" -> removerProjeto
      "l" -> visualizarProjetosPendentes
      "e" -> solicitarEntrada
      "m" -> chat
      "b" -> bancoDeAtividades
      "s" -> sairDoSistema
      _   -> erroMenuPrincipal

-- sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"


-- Função para cadastrar um novo usuário
cadastrarUsuario :: IO ()
cadastrarUsuario = do

    clearScreen

    putStrLn $ "Cadastro: " ++ "\n\n"
            ++ "Digite seu nome: "

    nome <- getLine

    ------------- checar usuario

    putStrLn "Digite sua senha: "
    senha <- getLine

    idUsuario <- randomRIO (1000, 9999 :: Int)

    criaUsuario (show(idUsuario)) nome senha
    menuPrincipal


-- Função para deletar um usuário
deletarUsuario :: IO()
deletarUsuario = do
  clearScreen
                            
  putStrLn "Digite seu id:"
  id <- getLine
  putStrLn "Digite sua senha:"
  senha <- getLine
  -- Tem que ter uma função para verificar se a senha bate com o nome do usuário
  removeUsuario id
  exitSuccess


-- Função para criar um projeto
cadastrarProjeto :: IO()
cadastrarProjeto = do

  clearScreen
  putStrLn $ "Cadastrar Projeto:" ++ "\n"
          ++ "Digite seu nome:"
  nomeUsuario <- getLine

  putStrLn "Digite um título para o projeto:"
  nomeProjeto <- getLine

  putStrLn "Digite a descrição do seu projeto:"
  descricao <- getLine

  idProjeto <- randomRIO (100, 999 :: Int)

  criaProjeto (show (idProjeto)) nomeProjeto descricao nomeUsuario


menuProjetos :: IO()
menuProjetos = do 

  putStrLn "Digite seu ID:"
  idUsuario <- getLine
  putStrLn "Digite o ID do projeto que deseja acessar:"
  idProjeto <- getLine

  -- let gerente = Util.ehGerente id "Projeto"

  -- if gerente then menuRestritoAtividades
  -- else menuPublicoAtividades

    -- verifica se é gerente e mostra o menu correspondente
  putStrLn ""


removerProjeto :: IO()
removerProjeto = do
    putStrLn "Digite seu ID:"
    idUsuario <- getLine
    putStrLn "Digite o ID do projeto que deseja excluir:"
    idProjeto <- getLine

    -- ehGerente 
    removeProjeto idProjeto
    putStrLn "Projeto removido com sucesso!"


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
