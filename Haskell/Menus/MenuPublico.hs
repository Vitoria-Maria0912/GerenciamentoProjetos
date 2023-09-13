module Menus.MenuPublico where
import Controllers.Atividades as Atividades
import Util.ClearScreen
import Data.Char (toLower)


-- Caso o usuário digite o comando errado retorna ao menu
erroMenuPublico :: IO()
erroMenuPublico =  do
    putStrLn $ "----------------------------------"
            ++ "Entrada Inválida. Tente novamente!" 
            ++ "----------------------------------\n"
    menuPublicoProjeto

-- Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :: IO()
menuPublicoProjeto = do 

    clearScreen

    putStrLn $ ".----------------------------------------------------------." ++ "\n"
            ++ "|                    Menu Projeto                          |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|                 Selecione uma opção:                     |" ++ "\n"
            ++ "|                                                          |" ++ "\n"
            ++ "|            I - Iniciar uma atividade                     |" ++ "\n"
            ++ "|            F - Finalizar uma atividade                   |" ++ "\n"
            ++ "|            V - Visualizar atividades do projeto          |" ++ "\n"
            ++ "|            A - Visualizar status de uma atividade        |" ++ "\n"
            ++ "|            O - Dar feedback em uma atividade             |" ++ "\n"
            ++ "|            S - Sair do sistema                           |" ++ "\n"
            ++ ".----------------------------------------------------------." ++ "\n"

    option <- getLine
    let lowerOption = map toLower option
    case lowerOption of 

        "i" -> comecarAtividade
        "f" -> finalizarAtividade
        "v" -> visualizarAtividades
        "a" -> statusAtividade
        "o" -> criarFeedback
        "s" -> sairDoSistema
        _   -> erroMenuPublico

-- Sai do sistema
sairDoSistema :: IO()
sairDoSistema = putStrLn "Você saiu do sistema! Até a próxima!"

-- Inicia uma atividade --> PODERIA COLOCAR AQUI SÓ A FUNÇÃO QUE MUDA O STATUS PRA PENDENTE
comecarAtividade :: IO()
comecarAtividade = do
    
    putStrLn "Digite o ID da atividade que deseja começar:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine

    -- let atividade = Projeto.getAtividade idAtividade

    -- if status atividade == "Não atribuída!" then 
    --     Atividades.mudaStatus "Pendente..."
    --     putStrLn $ "Você começou a atividade: " ++ atividade.titulo 

    -- else do
    putStrLn "Esta atividade já está em andamento!"

-- Finaliza uma atividade --> PODERIA COLOCAR AQUI SÓ A FUNÇÃO QUE MUDA O STATUS PRA CONCLUÍDO
finalizarAtividade :: IO()
finalizarAtividade = do

    putStrLn "Digite o ID da atividade que deseja finalizar:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine

    -- armazenar todas as atividades finalizadas daquele usuário
    -- decidir como armazenar isso

    -- let atividade = Projeto.getAtividade idAtividade

    -- Atividades.mudaStatus atividade "Concluída!"

    putStrLn "Atividade finalizada com sucesso!"
    
-- Mostra o status de uma atividade
statusAtividade :: IO()
statusAtividade = do

    putStrLn "Digite o ID da atividade que deseja visualizar o status:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine

    -- let atividade = Projeto.getAtividade idAtividade

    putStrLn "atividade.status"


visualizarAtividades :: IO()
visualizarAtividades = do 

    putStrLn "Digite o ID do projeto que deseja visualizar as atividade:"
    idProjeto <- getLine

    -- será possível ver apenas o nome ou, por exemplo, a quantidade de membros
    -- em cada atividade?

    -- let projeto = Projeto.getProjeto idProjeto
   
    putStrLn "projeto.exibeAtividades"

-- Função para criar feedback
criarFeedback :: IO ()
criarFeedback = do

    putStrLn "Digite seu ID:"
    idUsuario <- getLine
    putStrLn "Digite o ID da atividade que deseja dar Feedback:"
    idAtividade <- getLine
    putStrLn "Digite o ID do projeto que a atividade pertence:"
    idProjeto <- getLine
    putStrLn "Digite um breve comentário sobre a atividade:"
    comentario <- getLine





    -- let projeto = getProjeto idProjeto
    -- let atividade = Projeto.getAtividade idAtividade
    -- let usuario = getUsuario idUsuario

    -- if usuario ehGerente || usuario ehMembroResponsavel

    -- Atividades.adicionaFeedback atividade "comentário"
    
    putStrLn ""
