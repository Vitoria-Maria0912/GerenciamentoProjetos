module Menus.MenuGeral where
import qualified Data.Char as Char
import System.Exit (exitSuccess)
import System.Random 
import Data.Char (toLower)
import Util.ClearScreen
import Util.Util
import Controllers.Usuario
import Controllers.Projeto
import Menus.MenuGerente (menuRestritoProjeto)
import Menus.MenuPublico (menuPublicoProjeto)
import qualified Controllers.Usuario as Usuario
import qualified Control.Category as Usuario


-- Exibe erro e retorna ao menu
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn $ "------------------------------------------------------------\n"
          ++   "|            Entrada Inválida. Tente novamente!            |\n"
          ++   "------------------------------------------------------------\n"
    menuPrincipal


menuPrincipal :: IO ()
menuPrincipal = do

  putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "|                      Menu Principal                      |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|                   Selecione uma opção:                   |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|             G - Menu de projetos                         |" ++ "\n"
          ++ "|             C - Cadastrar novo usuário                   |" ++ "\n"
          ++ "|             D - Deletar perfil                           |" ++ "\n"
          ++ "|             P - Criar projeto                            |" ++ "\n"
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
      "l" -> visualizarProjetosPendentes
      "e" -> solicitarEntrada
      "m" -> chat
      "b" -> bancoDeAtividades
      "s" -> sairDoSistema
      _   -> erroMenuPrincipal 

  clearScreen

-- Sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"

-- Cadastra um usuário no sistema
cadastrarUsuario :: IO ()
cadastrarUsuario = do

    clearScreen

    putStrLn $ "Cadastro: " ++ "\n\n"
            ++ "Digite seu nome: "

    nome <- getLine

    putStrLn "Digite sua senha: "
    senha <- getLine

    idUsuario <- randomRIO (0000, 9999 :: Int)

    usuarios <- lerUsuarios "Database/LocalUsers/usuarios.txt"

    if (verificaIdUsuario (show(idUsuario)) usuarios == False) then do
        let usuario = Usuario {Usuario.idUsuario = show(idUsuario), Usuario.nome = nome, Usuario.senha = senha}
          -- getUsuario (show(idUsuario)) usuarios
        let novosUsuarios = adicionarUsuario usuario usuarios
        escreverUsuarios "Database/LocalUsers/usuarios.txt" novosUsuarios
        putStrLn $ "Usuário cadastrado com sucesso! Seu ID é " ++ show(idUsuario) ++ "\n"
        menuPrincipal
    else do
        putStrLn $ "O ID já existe na base de dados.\n"
        cadastrarUsuario

-- Deleta um usuário do sistema
deletarUsuario :: IO()
deletarUsuario = do
                            
  putStrLn "Digite seu id:"
  idUsuario <- getLine
  putStrLn "Digite sua senha:"
  senha <- getLine

  removeEAtualizaUsuarios idUsuario "Database/LocalUsers/usuarios.txt"
  menuPrincipal

 -- usuarios <- lerUsuarios "Database/LocalUsers/usuarios.txt"

  --if (verificaIdUsuario (show(idUsuario)) usuarios == True) then do
    --let usuario = getUsuario idUsuario usuarios

   -- if (verificaSenhaUsuario senha usuario == True) then do 
        -- remove.. 
        --putStrLn "Usuário deletado com sucesso!\n"
        --exitSuccess
    --else do
        --putStrLn "Senha ou ID incorretos! Tente novamente!\n"
        --menuPrincipal
  --else do 
        --putStrLn "ID inexistente! Tente novamente!\n"
        


  -- Tem que ter uma função para verificar se a senha bate com o nome do usuário
  
  clearScreen
  
-- Função para criar um projeto
cadastrarProjeto :: IO()
cadastrarProjeto = do

    putStrLn $ "Cadastrar Projeto:" ++ "\n"
            ++ "Digite seu nome:"
    nomeUsuario <- getLine

    putStrLn "Digite um título para o projeto:"
    nomeProjeto <- getLine

    putStrLn "Digite a descrição do seu projeto:"
    descricao <- getLine

    idProjeto <- randomRIO (100, 999 :: Int)

    projetos <- lerProjetos "dados/projetos.txt"

  -- if (verificaIdProjeto (show (idProjeto)) projetos == False) then do
  --     criaProjeto (show (idProjeto)) nomeProjeto descricao nomeUsuario Nothing Nothing
  --     let projeto = getProjeto (show (idProjeto)) projetos
  --     let novosProjetos = adicionarProjeto projeto projetos
  --     escreverProjetos "dados/projetos.txt" novosProjetos
  --     putStrLn $ "Projeto cadastrado com sucesso!\n"
  --     menuPrincipal
  -- else do
    putStrLn $ "O ID já existe na base de dados.\n"
    -- cadastrarProjeto

-- Verifica se o usuário é gerente e mostra o menu correspondente
menuProjetos :: IO()
menuProjetos = do 

    putStrLn "Digite seu ID:"
    idUsuario <- getLine
    putStrLn "Digite o ID do projeto que deseja acessar:"
    idProjeto <- getLine

    projetos <- lerProjetos "dados/projetos.txt"

    let projeto = getProjeto idProjeto projetos

    let gerente = ehGerente idUsuario projetos

    if gerente then menuRestritoProjeto
    else menuPublicoProjeto
    
-- Função para visualizar projetos pendentes
visualizarProjetosPendentes :: IO ()
visualizarProjetosPendentes = do
  -- Implementation logic for viewing pending projects
    putStrLn "Implementação em andamento."
    menuPrincipal


-- Função para solicitar entrada em um projeto
solicitarEntrada :: IO ()
solicitarEntrada = do
    putStrLn "Digite seu ID:"
    idUsuario <- getLine
    putStrLn "Digite o ID do projeto que deseja ingressar:"
    idProjeto <- getLine

    projetos <- lerProjetos "dados/projetos.txt"

    let projeto = getProjeto idProjeto projetos

    putStrLn "Implementação em andamento."
    menuPrincipal

-- Entra no chat
chat :: IO ()
chat = do
  -- Implementation logic for entering the chat
  putStrLn "Implementação em andamento."
  menuPrincipal


-- Visualiza atividades cadastradas no sistema
bancoDeAtividades :: IO ()
bancoDeAtividades = do
  -- Implementation logic for viewing activity bank
  putStrLn "Implementação em andamento."
  menuPrincipal

