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


menuProjeto :: IO()
menuProjeto = 
    let gerente = False -- tem que fazer a análise

    | gerente = menuRestritoAtividades
    | otherwise = menuPublicoAtividades
    

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


-- função para receber as entradas do usuário, referente a criação do perfil
cadastrarUsuario:: IO()
cadastrarUsuario = do
    putStrLn "Olá! Qual o seu nome?"
    nome <- getLine
    
    if FuncoesAuxiliares.verificaNomeUsuario nome then
        putStrLn "Já tem usuário com esse nome!"
        exitSistem
    else do
        putStrLn "Ótimo! Agora defina a sua senha!"
        password <- getLine
        id <- FuncoesAuxiliares.geraIDUsuario
        Usuario.cadastraUsuario id password nome
        putStrLn $ "\nParabéns, " ++ nome
            ++ ", você está cadastrado(a) no Sistema de Gerenciamento de Projetos!"
        main


-- função para receber as entradas do usuário, referente a exclusão de um perfil
deletarUsario :: IO()
deletarUsario = do
    putStrLn "Digite o nome do seu perfil: "
    name <- getLine
    putStrLn "Digite sua senha: "
    senha <- getLine
    
    if FuncoesAuxiliares.verificaNomeUsuario name
        && FuncoesAuxiliares.verificaSenhaUsuario senhathen then do
        Usuario.removerUsuario senha
        putStrLn $ name ++ ", seu perfil foi deletado com sucesso!"
    else do
        putStrLn "Não há usuário com esse nome ou senha!"


-- função para receber as entradas referentes a criação de projeto
cadastrarProjeto :: IO()
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


-- recebe as entradas referentes a remoção de um projeto
removerProjeto :: IO()
removerProjeto = do
    putStrLn "Digite o nome do projeto que deseja deletar:"
    nome <- getLine
    
    | not (FuncoesAuxiliares.verificaNomeProjeto nome) = putStrLn "Projeto inexistente!"
    | otherwise = do
        putStrLn "Digite sua senha: "
        senha <- getLine
        if FuncoesAuxiliares.verificaSenhaUsuario senha
            then putStrLn "Projeto removido com sucesso."
        else
            putStrLn "Você não pode executar essa ação."


-- captura os dados de um usuário que quer entrar em um projeto
solicitarEntrada :: IO ()
solicitarEntrada = do
    putStrLn "Solicitar Entrada em Projeto:\n"
    putStrLn "Digite o ID do projeto que deseja entrar: "
    idProjeto <- readLn :: IO Int
    
    if FuncoesAuxiliares.verificaIDProjeto idProjeto then do
        -- funcao para solicitar entrada em projeto
        putStrLn "Solicitação enviada com sucesso!"
    else putStrLn "Projeto inexistente"


-- função para receber as entradas de um feedback em uma atividade
criarFeedback :: IO ()
criarFeedback = do
    putStrLn "Dar Feedback de Atividade Realizada:\n"
    putStrLn "Digite o ID da atividade: "
    idAtividade <- readLn :: IO Int
    
    if FuncoesAuxiliares.verificaIDAtividade idAtividade then do
        putStrLn "Digite o seu feedback: "
        feedback <- getLine
        -- chamar funcao para adicionar feedback
        putStrLn "Feedback adicionado com sucesso!"
    else putStrLn "Atividade inexistente"


-- função para iniciar o chat
chat :: IO ()
chat = do
    putStrLn "Abrir Caixa de Mensagens:\n"
    putStrLn "Digite o nome do destinatário: "
    destinatario <- getLine
    putStrLn "Digite a mensagem: "
    mensagem <- getLine
    -- Chame a função para enviar a mensagem ao destinatário
    putStrLn "Mensagem enviada com sucesso!"


-- função para visualizar o banco de atividades
bancoDeAtividades :: IO ()
bancoDeAtividades = do
    putStrLn "Visualizar Banco de Atividades:\n"
    -- Chame a função para listar as atividades do banco
    putStrLn "Atividades disponíveis no banco:"


-- Função para listar projetos em andamento (L - Listar projetos)
visualizarProjetosPendentes :: IO ()
visualizarProjetosPendentes = do
    putStrLn "Listar Projetos em Andamento:\n"
    -- Chame a função para listar os projetos em andamento
    putStrLn "Projetos em andamento:"


menuRestritoAtividades :: IO()
menuRestritoAtividades = do 

    -- só quem terá acesso a este menu é o gerente do projeto

    putStrLn "O que deseja fazer agora?\n"

            ++ "C - criar uma atividade\n"
            ++ "G - gerenciar membros do projeto\n"
            ++ "R - remover uma tarefa\n"
            ++ "P - voltar ao menu do projeto\n"
            ++ "M - voltar ao menu principal\n"
            ++ "S - sair do sistema\n"

            ++ "\nEscolha uma opção: "

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "c" -> criaAtividade
        "g" -> gerenciarMembros
        "r" -> removeAtividade
        "p" -> menuProjeto
        "m" -> menuPrincipal
        "s" -> exitSistem
        _   -> erroMenuProjeto


criaAtividade :: IO()
criaAtividade = do
    putStrLn "Digite um título para sua atividade:"
    title <- getLine

    putStrLn "Descreva, brevemente, o que se deve realizar para concluir esta atividade."
    descricao <- getLine

    Atividades.mudaStatus "Não Atribuída!"
    
    putStrLn "Tarefa criada com sucesso!"


removeAtividade :: IO()
removeAtividade = do
    putStrLn "Digite o nome da atividade que deseja remover:"
    title <- getLine
    putStrLn "Atividade removida com sucesso!"


menuPublicoAtividades :: IO()
menuPublicoAtividades = do 

    -- todos os usuários tem acesso

    putStrLn "O que deseja fazer agora?\n"
            ++ "I - iniciar uma atividade\n"
            ++ "F - finalizar uma atividade\n"
            ++ "V - visualizar as tarefas do projeto\n"
            ++ "A - visualizar status de uma atividade"
            ++ "T - modificar status de uma atividade"
            ++ "P - voltar ao menu do projeto\n"
            ++ "M - voltar ao menu principal\n"
            ++ "S - sair do sistema\n"

            ++ "\nEscolha uma opção: "

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarTarefas
        "a" -> statusAtividade
        "p" -> menuProjeto
        "m" -> menuPrincipal
        "s" -> sairDoSistema
        _   -> erroMenuProjeto


comecarAtividade :: IO()
comecarAtividade = do
    
    putStrLn "Digite o nome da atividade que deseja começar:"
    title <- getLine

    Atividades.mudaStatus "Pendente..."

    putStrLn $ "Você começou a atividade: " ++ title


finalizarAtividade :: IO()
finalizarAtividade = do

    putStrLn "Digite o nome da atividade que deseja finalizar:"
    title <- getLine

    -- armazenar em um arquivo todas as atividades finalizadas daquele usuário
    
    Atividades.mudaStatus "Concluída!"

    putStrLn "Atividade finalizada com sucesso!"


statusAtividade :: IO()
statusAtividade = do

    putStrLn "Digite o nome da atividade que deseja visualizar o status:"
    title <- getLine

    Atividades.mostra statusAtividade --- e os parâmetros


visualizarAtividades :: IO()
visualizarAtividades = do 
   
    putStrLn "Projeto.exibeAtividades projeto"
