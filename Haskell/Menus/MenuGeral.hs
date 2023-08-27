module Haskell.Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import qualified Haskell.Projeto
import qualified Haskell.Usuario
import qualified Haskell.Atividades
import Haskell.Projeto
import qualified Haskell.FuncoesAuxiliares as FuncoesAuxiliares
import Data.Char (toLower)
import qualified Haskell.Usuario as Usuario
import qualified Haskell.Projeto as Projeto
import Haskell.Util.ClearScreen

-- main
menuGeral :: IO()
menuGeral = do
    menuPrincipal
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
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
  putStrLn "Digite seu username: "
  name <- getLine
  putStrLn "Digite seu nome: "
  username <- getLine
  putStrLn "Digite sua senha: "
  password <- getLine
  putStrLn "Digite sua descrição: "
  desc <- getLine
  createUser name username password desc
  menuPrincipal


-- Função para deletar um usuário
deletarUsuario :: IO ()
deletarUsuario = do
    putStrLn "Digite o nome do seu perfil: "
    nome <- getLine
    putStrLn "Digite sua senha: "
    senha <- getLine
    -- deveria passar a senha abaixo, para verificar
    if FuncoesAuxiliares.verificaNomeUsuario nome then do
        -- aqui invoca a função em Usuário, para remover
        putStrLn $ nome ++ ", seu perfil foi deletado com sucesso!"
    else do
        putStrLn "Não há usuário com esse nome ou senha!"


-- Função para criar um projeto
cadastrarProjeto :: IO ()
cadastrarProjeto = do
    putStrLn "Vamos criar o seu projeto!"
    putStrLn ".........................."
    putStrLn "Qual o título do projeto?"
    titulo <- getLine
    if FuncoesAuxiliares.verificaNomeProjeto titulo then do
        putStrLn "Nome já utilizado em outro projeto!"
        menuPrincipal
    else do
        putStrLn "\nDescreva, brevemente, seu projeto!"
        descricao <- getLine
        -- aqui invoca a função em projeto, para cadastrar
        putStrLn "Projeto criado!"


-- Função para remover um projeto
removerProjeto :: IO ()
removerProjeto = do
    putStrLn "Digite o nome do projeto que deseja deletar:"
    nome <- getLine
    if not (FuncoesAuxiliares.verificaNomeProjeto nome) then putStrLn "Projeto inexistente!"
    else do
        putStrLn "Digite sua senha: "
        senha <- getLine
        -- Abaixo, deveria ser: FuncoesAuxiliares.verificaSenhaUsuario senha
        if True then putStrLn "Projeto removido com sucesso."
        else putStrLn "Você não pode executar essa ação."


-- Função para solicitar entrada em um projeto
solicitarEntrada :: IO ()
solicitarEntrada = do
    putStrLn "Solicitar Entrada em Projeto:\n"
    putStrLn "Digite o ID do projeto que deseja entrar: "
    idProjeto <- readLn :: IO Int
    -- Deveria ter uma função, talvez em FuncoesAuxiliares, para verificar o ID
    if True then do
        putStrLn "Solicitação enviada com sucesso!"
    else putStrLn "Projeto inexistente"


-- Função para dar feedback de uma atividade realizada
criarFeedback :: IO ()
criarFeedback = do
    putStrLn "Dar Feedback de Atividade Realizada:\n"
    putStrLn "Digite o ID da atividade: "
    idAtividade <- readLn :: IO Int
    -- Deveria ter uma função, talvez em FuncoesAuxiliares, para verificar o ID
    if True then do
        putStrLn "Digite o seu feedback: "
        feedback <- getLine
        -- Chame a função para adicionar feedback
        putStrLn "Feedback adicionado com sucesso!"
    else putStrLn "Atividade inexistente"


-- Função para abrir a caixa de mensagens
chat :: IO ()
chat = do
    putStrLn "Abrir Caixa de Mensagens:\n"
    putStrLn "Digite o nome do destinatário: "
    destinatario <- getLine
    putStrLn "Digite a mensagem: "
    mensagem <- getLine
    -- Chame a função para enviar a mensagem ao destinatário
    putStrLn "Mensagem enviada com sucesso!"


-- Função para visualizar o banco de atividades
bancoDeAtividades :: IO ()
bancoDeAtividades = do
    putStrLn "Visualizar Banco de Atividades:\n"
    -- Chame a função para listar as atividades do banco
    putStrLn "Atividades disponíveis no banco:"


-- Função para listar projetos em andamento
visualizarProjetosPendentes :: IO ()
visualizarProjetosPendentes = do
    putStrLn "Listar Projetos em Andamento:\n"
    -- Chame a função para listar os projetos em andamento
    putStrLn "Projetos em andamento:"

