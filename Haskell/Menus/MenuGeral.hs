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

-- | Função que cadastra um usuário no sistema
cadastrarUsuario :: IO ()
cadastrarUsuario = do

    clearScreen

    let userFilePath = "Database/usuarios.json"

    putStrLn $ "Cadastro: " ++ "\n\n"
            ++ "Digite seu nome: "

    nome <- getLine

    putStrLn "Digite sua senha: "
    senha <- getLine

    idUsuario <- randomRIO (0000, 9999 :: Int)

    let usuarioNoSistema = (getUsuario idUsuario (getUsuarios userFilePath))

    case (usuarioNoSistema) of
      Just _ -> do
        putStrLn $ "------------------------------------------------------------." ++ "\n"
                ++ "|             Falha no cadastro! Tente novamente.            |" ++ "\n"
                ++  "------------------------------------------------------------" ++ "\n"
        cadastrarUsuario

      Nothing -> do
        salvarUsuario userFilePath idUsuario nome senha
        putStrLn $ "Usuário cadastrado com sucesso! Seu ID é " ++ show(idUsuario) ++ "\n"
        menuPrincipal

-- | Função que deleta um usuário do sistema
deletarUsuario :: IO()
deletarUsuario = do
  let userFilePath = "Database/usuarios.json"
                            
  putStrLn "Digite seu id:"
  idUsuario <- readLn :: IO Int
  putStrLn "Digite sua senha:"
  senha <- getLine

  let usuarioNoSistema = (getUsuario idUsuario (getUsuarios userFilePath))

  case (usuarioNoSistema) of
    Just _ -> do
      removerUsuario userFilePath idUsuario
      putStrLn("Usuário removido com sucesso!")
      menuPrincipal

    Nothing -> do
       putStrLn $ "------------------------------------------------------------." ++ "\n"
                ++"|            Falha ao tentar remover! Tente novamente.      |" ++ "\n"
                ++ "------------------------------------------------------------" ++ "\n"
      deletarUsuario
                
  
-- | Função para criar um projeto
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

    putStrLn "Você gostaria de adicionar membros no seu projeto agora? (s/n)" 
    addMembro <- getLine

    let usuarios = getUsuarios "Controllers/usuarios.json"

      if addMembro == "s" then do
        putStrLn $ "Digite o ID do(s) usuário(s) que você deseja adicionar de acordo com a lista abaixo:" ++ "\n"
        
        mapM_ imprimirUsuario usuarios

        putStrLn "Digite o(s) ID(s) que deseja adicionar:"
        usuariosAadd <- getLine
         -- provavelmente isso vai ser uma lista de 1 ou mais IDs que precisarão ser adicionados no projeto.
         -- chamar aqui função que add o membro no projeto
        putStrLn "Membros cadastrados com sucesso!"
      else
        putStrLn "Ok, você poderá adicionar os membros posteriormente"
    
    putStrLn "Você gostaria de adicionar atividades no seu projeto agora? (s/n)"
    addAtv <- getLine  

      if addAtv == "s" then do
         -- retorna as atividades do banco com ids
        -- recebe o ids das atividades que gostaria de adicionar
        -- adiciona as atividades no projeto pelo ID
      else 
        putStrLn "Ok, você poderá adicionar as atividades posteriormente"


  -- if (verificaIdProjeto (show (idProjeto)) projetos == False) then do
  --     criaProjeto (show (idProjeto)) nomeProjeto descricao nomeUsuario Nothing Nothing
  --     let projeto = getProjeto (show (idProjeto)) projetos
  --     let novosProjetos = adicionarProjeto projeto projetos
  --     escreverProjetos "dados/projetos.txt" novosProjetos
  --     putStrLn $ "Projeto cadastrado com sucesso!\n"
  --     menuPrincipal
  -- else do
    --putStrLn $ "O ID já existe na base de dados.\n"
    -- cadastrarProjeto

-- | Função que verifica se o usuário é gerente e mostra o menu correspondente
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
    
-- | Função para visualizar projetos pendentes
visualizarProjetosPendentes :: IO ()
visualizarProjetosPendentes = do
  -- Implementation logic for viewing pending projects
    putStrLn "Implementação em andamento."
    menuPrincipal

-- | Função para entrar no chat
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

