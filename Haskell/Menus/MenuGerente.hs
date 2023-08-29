{-
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

-}
