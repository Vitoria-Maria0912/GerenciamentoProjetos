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

    numUsuarios <- getNumDeUsuarios "Controllers/dados.json"

    putStrLn $ "Cadastro: " ++ "\n\n"
            ++ "Digite seu nome: "

    nome <- getLine

    putStrLn "Digite sua senha: "
    senha <- getLine

    idUsuario <- randomRIO (0, 10 :: Int)

    -- falta colocar verificações

    salvarUsuario "Controllers/dados.json" idUsuario nome senha
    numUsuariosAtt <- getNumDeUsuarios "Controllers/dados.jason"
    if numUsuariosAtt > numUsuarios then do -- verificacao TEMPORÁRIA (melhorar depois)
      putStrLn ("Usuário cadastrado com sucesso!") -- adicionar o ID aqui depois
      menuPrincipal
    else do
      putStrLn ("Por favor, tente novamnete")
      cadastrarUsuario

-- Deleta um usuário do sistema
deletarUsuario :: IO()
deletarUsuario = do
                            
  putStrLn "Digite seu id:"
  idUsuario <- getLine
  putStrLn "Digite sua senha:"
  senha <- getLine

  numUsuarios <- getNumDeUsuarios "Controllers/dados.json"

-- FALTA VERIFICAÇÃO DA SENHA E SE O ID EXISTE
  removerUsuario "Controllers/dados.json" idUsuario

  numUsuariosAtt <- getNumDeUsuarios "Controllers/dados.jason"

  -- VERIFICAÇÃO TEMPORÁRIA -> MUDAR PRA VERIFICAR POR ID
  if numUsuariosAtt < numUsuarios then do  
    putStrLn("Usuário removido com sucesso!")
    menuPrincipal
  else do
     putStrLn ("Por favor, tente novamente")
     cadastrarUsuario
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

