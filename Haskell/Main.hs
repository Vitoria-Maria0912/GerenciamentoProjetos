-- main = entrada e saida, chama a funçao dos outros modulos que tem as funções.

module Haskell.Main where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import qualified Haskell.Projeto
import qualified Haskell.Usuario
import qualified Haskell.Atividades
import Haskell.Projeto (Project(senha))
import qualified Haskell.FuncoesAuxiliares as FuncoesAuxiliares
import Data.Char (toLower)


-- main
main :: IO()
main = do
    menuPrincipal
    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of
        "c" -> cadastrarUsuario
        "d" -> deletarUsario
        "p" -> cadastrarProjeto
        "r" -> removerProjeto
        "l" -> visualizarProjetosPendentes
        "e" -> solicitarEntrada
        "f" -> criarFeedback
        "m" -> chat
        "b" -> bancoDeAtividades
        "s" -> sairDoSistema
        _   -> erroMenuPrincipal


-- Função para imprimir "S G P" de forma estilizada
imprimirSGP :: IO ()
imprimirSGP = do
    putStrLn "......   ......    ......" ++ "/n"
          ++ ".        .         .    ." ++ "/n"
          ++ "......   .  ...    ......" ++ "/n"
          ++ "     .   .    .    .     " ++ "/n"
          ++ "......   ......    .     " ++ "/n"


-- função que imprime o menu
menuPrincipal :: IO()
menuPrincipal = do
    imprimirSGP
    putStrLn "Menu Principal:\n"
    putStrLn $ "C - criar perfil\n"
            ++ "D - deletar perfil\n"
            ++ "P - criar projeto\n"
            ++ "R - remover projeto\n"
            ++ "L - listar projetos\n"
            ++ "E - solicitar entrada em um projeto\n"
            ++ "F - dar feedback de uma atividade realizada"
            ++ "M - abrir caixa de mensagens"
            ++ "B - visualizar banco de atividades"
            ++ "S - sair do sistema\n"
            ++ "\nEscolha uma opção: "


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn   "----------------------------------"
    putStrLn   "Entrada Inválida. Tente novamente!"
    putStrLn   "----------------------------------\n"
    main


-- sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"


-- Função para cadastrar um novo usuário
cadastrarUsuario :: IO ()
cadastrarUsuario = do
    putStrLn "Olá! Qual o seu nome?"
    nome <- getLine
    if FuncoesAuxiliares.verificaNomeUsuario nome then do
        putStrLn "Já existe um usuário com esse nome!"
        main
    else do
        putStrLn "Ótimo! Agora defina a sua senha!"
        senha <- getLine
        id <- FuncoesAuxiliares.geraIDUsuario
        Usuario.cadastraUsuario id senha nome
        putStrLn $ "\nParabéns, " ++ nome
            ++ ", você está cadastrado no Sistema de Gerenciamento de Projetos!"
        main


-- Função para deletar um usuário
deletarUsuario :: IO ()
deletarUsuario = do
    putStrLn "Digite o nome do seu perfil: "
    nome <- getLine
    putStrLn "Digite sua senha: "
    senha <- getLine
    if FuncoesAuxiliares.verificaNomeUsuario nome && FuncoesAuxiliares.verificaSenhaUsuario senha then do
        Usuario.removerUsuario nome
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
        id <- FuncoesAuxiliares.geraIDProjeto
        Projeto.cadastraProjeto id titulo descricao
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
        if FuncoesAuxiliares.verificaSenhaUsuario senha then putStrLn "Projeto removido com sucesso."
        else putStrLn "Você não pode executar essa ação."


-- Função para solicitar entrada em um projeto
solicitarEntrada :: IO ()
solicitarEntrada = do
    putStrLn "Solicitar Entrada em Projeto:\n"
    putStrLn "Digite o ID do projeto que deseja entrar: "
    idProjeto <- readLn :: IO Int
    if FuncoesAuxiliares.verificaIDProjeto idProjeto then do
        putStrLn "Solicitação enviada com sucesso!"
    else putStrLn "Projeto inexistente"


-- Função para dar feedback de uma atividade realizada
criarFeedback :: IO ()
criarFeedback = do
    putStrLn "Dar Feedback de Atividade Realizada:\n"
    putStrLn "Digite o ID da atividade: "
    idAtividade <- readLn :: IO Int
    if FuncoesAuxiliares.verificaIDAtividade idAtividade then do
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

