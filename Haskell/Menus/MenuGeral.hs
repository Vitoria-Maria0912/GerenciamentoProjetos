module Haskell.Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import Data.Char (toLower)
import Haskell.Util.ClearScreen
import Haskell.Controllers.Usuario

-- main
menuGeral :: IO()
menuGeral = do
    menuPrincipal
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
        "c" -> cadastro
        --"d" -> deletarUsuario
        --"p" -> cadastrarProjeto
        --"r" -> removerProjeto
        --"l" -> visualizarProjetosPendentes
        --"e" -> solicitarEntrada
        --"f" -> criarFeedback
        --"m" -> chat
        --"b" -> bancoDeAtividades
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
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | oooooooo8     o      ooooooooo      o       oooooooo8 ooooooooooo oooooooooo    ooooooo    |"
  putStrLn " |o888     88    888      888    88o   888     888        88  888  88  888    888 o888   888o |"
  putStrLn " |888           8  88     888    888  8  88     888oooooo     888      888oooo88  888     888 |"
  putStrLn " |888o     oo  8oooo88    888    888 8oooo88           888    888      888  88o   888o   o888 |"
  putStrLn " | 888oooo88 o88o  o888o o888ooo88 o88o  o888o o88oooo888    o888o    o888o  88o8   88ooo88   |"
  putStrLn " |                                                                                            |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
  putStrLn "Digite seu nome: "
  nome <- getLine
  putStrLn "Digite sua senha: "
  senha <- getLine
  criaUsuario nome senha
  menuPrincipal