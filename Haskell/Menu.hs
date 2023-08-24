import Data.Char (toLower)


-- caso o usuário digite o comando errado
erroMenuPrincipal :: IO()
erroMenuPrincipal = do
    putStrLn   "----------------------------------"
    putStrLn $ "Entrada Inválida. Tente novamente!" 
    putStrLn   "----------------------------------\n"
    menuPrincipal


-- primeiro menu mostrado ao usuário
menuPrincipal :: IO()
menuPrincipal = do

    putStrLn "Menu Principal:\n"
    putStrLn $ "C - criar perfil\n"
            ++ "D - deletar perfil\n"
            ++ "P - criar projeto\n"
            ++ "R - remover projeto\n"
            ++ "L - listar projetos\n"
            ++ "E - solicitar entrada em um projeto\n"
            ++ "S - sair do sistema\n"
            ++ "\nEscolha uma opção: "
    option <- getLine
    putStrLn ""

    let lowerOption = map toLower option
    case lowerOption of 
        -- está como String, mas serão as funções
        "c" -> putStrLn "createProfile"
        "d" -> putStrLn "deleteProfile" 
        "p" -> putStrLn "createProject"
        "r" -> putStrLn "removeProject"
        "l" -> putStrLn "viewProjectsInProgress"
        "e" -> putStrLn "requestEntry"
        "s" -> exitSistem
        _   -> erroMenuPrincipal


-- sai do sistema
exitSistem :: IO()
exitSistem = putStrLn "Você saiu do sistema! Até a próxima!"
     

-- função para receber as entradas do usuário, referente a criação do perfil
createProfile :: IO()
createProfile = do
    putStrLn "Olá! Qual o seu nome?"
    name <- getLine
    -- criar uma função de amazenamento
    putStrLn $ "\nParabéns, " ++ name  
    ++ ", você está cadastrado(a) no Sistema de Gerenciamento de Projetos!"
    menuPrincipal


-- função para receber as entradas do usuário, referente a exclusão de um perfil
deleteProfile :: IO()
deleteProfile = do
    putStrLn "Digite o nome do seu perfil: "
    name <- getLine
    putStrLn "Digite sua senha: "
    senha <- getLine
    -- tem fazer uma função para remover do sistema
    putStrLn $ name ++ ", seu perfil foi deletado com sucesso!"


-- função para receber as entradas referentes a criação de projeto
criaProject :: IO()
criaProject = do
    putStrLn "Vamos criar o seu projeto!"
    putStrLn "Qual o título do projeto?"
    nome <- getLine    
    -- FUNÇÃO -> checa se já existe esse projeto com esse nome
    if (Project.Utils.verificaNome nome == False) then do
        putStrLn "Projeto já existente, por favor escolha outro nome."
    else do
        putStrLn "Me diz uma descrição para o seu projeto:"
        descricao <- getLine
        putStrLn "Para completar, me diz sua senha:"
        senha <- getLine
    {-
        gerará o ID do projeto
        let idProjeto = Project.Utils.gerarID
        Falta criar Projects que irá manipular os dados (TXT ou CSV)
        Project irá colocar no arquivo o id como "chave de acesso",
        depois salvará os arquivos obtidos com a Interação com o usuário
        ProjectUtils.WriteProjeto(Projects.criaProject(read(idProjeto)) nome descricao senha)
    -}
    putStrLn "Projeto criado!"


-- recebe as entradas referentes a remoção de um projeto
removeProject :: IO()
removeProject = do
    putStrLn "Digite o nome do projeto que deseja deletar."
    nome <- getLine
    -- checar se o projeto existe
    let naoExiste = False -- <<<< é só para compilar 
    if naoExiste 
        then putStrLn "Projeto inexistente!"
    else do
    -- tem que ter uma verificação para saber se o usuário é gerente
        let gerente = False  -- <<<< é só para compilar 
        if gerente
            then putStrLn "Projeto removido com sucesso."
        else 
            putStrLn "Você não pode executar essa ação. Você precisa ser gerente para realizá-la!"
