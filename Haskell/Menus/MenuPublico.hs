module Menus.MenuPublicoProjeto where
import Controllers.Atividades as Atividades
import Util.ClearScreen
import Data.Char (toLower)

-- todos os usuários tem acesso

-- caso o usuário digite o comando errado volta para o menu
erroMenuPublico :: IO()
erroMenuPublico =  do
    putStrLn   "----------------------------------"
    putStrLn $ "Entrada Inválida. Tente novamente!" 
    putStrLn   "----------------------------------\n"
    menuPublicoAtividades


menuPublicoAtividades :: IO()
menuPublicoAtividades = do 

    clearScreen

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                     Menu Projeto                          |" ++ "\n"
            ++ "|                                                           |" ++ "\n"
            ++ "|                  Selecione uma opção:                     |" ++ "\n"
            ++ "|                                                           |" ++ "\n"
            ++ "|             I - Iniciar uma atividade                     |" ++ "\n"
            ++ "|             F - Finalizar uma atividade                   |" ++ "\n"
            ++ "|             V - Visualizar atividades do projeto          |" ++ "\n"
            ++ "|             A - Visualizar status de uma atividade        |" ++ "\n"
            ++ "|             O - Dar feedback em uma atividade             |" ++ "\n"
            ++ "|             M - Voltar ao menu principal                  |" ++ "\n"
            ++ "|             S - Sair do sistema                           |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criarFeedback
        "m" -> menuPrincipal
        "s" -> sairDoSistema
        _   -> erroMenuPublico


comecarAtividade :: IO()
comecarAtividade = do
    
    -- acessaria pelo nome ou pelo ID?
    putStrLn "Digite o nome da atividade que deseja começar:"
    titulo <- getLine

    -- se o usuário já está fazendo ela
    -- poderia fazer a checagem:

    -- if Atividades.mostraStatus == "Não atribuída!" then 
    --     Atividades.mudaStatus "Pendente..."
    --     putStrLn $ "Você começou a atividade: " ++ titulo

    -- else do
    putStrLn "Esta atividade já está em andamento!"

    -- decidir como armazenar isso


finalizarAtividade :: IO()
finalizarAtividade = do

    -- acessaria pelo nome ou pelo ID?
    putStrLn "Digite o nome da atividade que deseja finalizar:"
    titulo <- getLine

    -- armazenar todas as atividades finalizadas daquele usuário
    -- decidir como armazenar isso

    -- Atividades.mudaStatus "Concluída!"

    putStrLn "Atividade finalizada com sucesso!"


visualizarAtividades :: IO()
visualizarAtividades = do 

    -- como acessar as atividades do projeto sem precisar digitar o título dele?
    -- seria bom uma maneira de 'exibir o arquivo'
    -- mas será possível ver apenas o nome ou, por exemplo, a quantidade de membros
    -- em cada atividade?
   
    putStrLn "Projeto.exibeAtividades projeto"


statusAtividade :: IO()
statusAtividade = do

    -- acessaria pelo nome ou pelo ID?
    putStrLn "Digite o nome da atividade que deseja visualizar o status:"
    titulo <- getLine

    -- tem que passar atividade, mas acho que seria melhor pelo ID
    putStrLn "Atividades.mostraStatus titulo"


-- Função para criar feedback
criarFeedback :: IO ()
criarFeedback = do
  -- Implementation logic for creating feedback
  putStrLn "Implementação em andamento."
  menuPrincipal
