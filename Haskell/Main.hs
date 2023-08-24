import System.IO ()
import System.Exit (exitSuccess)

main :: IO ()
main = do
  
  putStrLn "Escolha uma opção:"
  putStrLn "1. Criar perfil"
  putStrLn "2. Remover perfil"
  putStrLn "3. Criar projeto"
  putStrLn "4. Remover um projeto"
  putStrLn "5. Listar os projetos em andamento"
  putStrLn "6. Solicitar entrada em um projeto"
  putStrLn "7. Criar atividade"
  putStrLn "8. Visualizar o banco de atividades"
  putStrLn "9. Ir para a caixa de mensagens"
  putStrLn "10. Sair do sistema"
  
  opcao <- getLine
  case opcao of 
    "1" -> criarPerfil
    "2" -> removerPerfil
    "3" -> criarProjeto
    "4" -> removerProjeto
    "5" -> listarProjetos
    "6" -> entrarProjeto
    "7" -> criarAtividade
    "8" -> bancoAtividades
    "9" -> caixaMensagens
    "10" -> exitSuccess
    _ -> putStrLn "Opção inválida!"
  putStrLn "Digite uma das opções apresentadas:"
  main


criarPerfil :: IO()
criarPerfil = do
  putStr "Insira o nome de usuário: "
  name <- getLine
  putStr "Insira a senha: "
  password <- getLine 
  id <- 0 -- Aqui teria a função para gerar o ID
  -- Aqui teria a função para criar o perfil, que receberia o nome, password e id
  putStr "Seu ID é: " + id


removerPerfil :: IO()
removerPerfil = do
  putStr "Insira sua senha: "
  password <- getLine
  putStr "Insira seu ID: "
  id <- getLine  -- Função para
  -- Aqui teria a função para remover o perfil, que receberia a senha e o ID
  -- A função pode retornar um booleano
  if True -- Quando a função estiver pronta, podemos fazer: if <funcao>
    then putStr "Perfil removido!"
    else putStr "Algo deu errado."
  