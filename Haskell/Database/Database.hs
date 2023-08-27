{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Haskell.Database.Database where
import System.IO
import System.Directory
import Control.Monad (filterM)
import Haskell.Util.AbrirFecharArquivo

directoryDatabase :: String
directoryDatabase = "./Modules/Database/LocalUsers/" -- Função que retorna o local padrão dos users criados

--Funções relacionadas a users
createUserDatabase :: String -> String -> String -> String -> IO()
createUserDatabase username name password description = do -- Função que cria um novo usuário no banco de dados
    let user = [username, name, password, description] -- seta 'user' como uma lista que guarda os parâmetros passados
    createDirectory (directoryDatabase ++ username)
    createDirectory (directoryDatabase ++ "/" ++ username ++ "/" ++ "listas")
    createDirectory (directoryDatabase ++ "/" ++ username ++ "/" ++ "sharedWithMe")
    withFile (directoryDatabase ++ username ++ "/" ++ username ++ ".txt") WriteMode $ \handle -> do
        hPutStrLn handle (unlines user)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username

deleteUserDatabase :: String -> IO()
deleteUserDatabase username = do -- Função para deletar um user do database
    removeFile (directoryDatabase++username++"/"++username++".txt") -- usa uma função de deletar um arquivo passando o caminho do arquivo

getNameDatabase :: String -> IO String
getNameDatabase username = do -- Função que retorna o nome de um usuário
    conteudo <- readFile (directoryDatabase++username++"/"++username ++ ".txt") -- o termo 'conteudo' recebe os dados lidos no txt
    let linhas = lines conteudo -- seta 'linhas' como uma lista onde cada termo é uma linha de 'conteudo'
    return (linhas !! 1) -- retorna o termo 1 de 'linhas' que é o Nome do user

getDescriptionDatabase :: String -> IO String
getDescriptionDatabase username = do -- Função que retorna a descrição de um usuário
    conteudo <- readFile (directoryDatabase++username++"/"++username ++ ".txt")
    let linhas = lines conteudo
    return (linhas !! 3)

editUserDatabase :: String -> String -> String -> String -> IO()
editUserDatabase username name password description = do
    let user = [username, name, password, description]
    withFile (directoryDatabase ++ username ++ "/" ++ username ++ ".txt") WriteMode $ \handle -> do
        hPutStrLn handle (unlines user)

--funções relacionadas ao login
loginDatabase :: String -> String -> IO Bool
loginDatabase username password = do
    let fileName = directoryDatabase ++ username++"/"++username ++ ".txt"
    userExists <- doesFileExist fileName
    if userExists then do
        conteudo <- readFile (directoryDatabase++username++"/"++username ++ ".txt")
        let linhas = lines conteudo
        if linhas !! 2 == password then return True
        else return False
    else return False

--funções relacionadas a listas
createToDoListDatabase :: String -> String -> String -> IO()
createToDoListDatabase username listName listdesc = do
    let listcontent = [listName, listdesc]
    existFile <- doesDirectoryExist (directoryDatabase ++ username ++ "/listas"++"/"++listName)
    if not existFile then do
        createDirectory (directoryDatabase ++ username ++ "/listas"++"/"++listName)
        withFile (directoryDatabase ++ username ++ "/listas/" ++ listName ++ "/" ++ "0" ++ listName ++ ".txt") WriteMode $ \handle -> do
            hPutStrLn handle (unlines listcontent)
    else return ()

deleteToDoListDatabase :: String -> String -> IO()
deleteToDoListDatabase username listName = do
    let filePath = directoryDatabase ++ username ++ "/listas/" ++ listName
    removeDirectoryRecursive filePath

addUserToListDatabase :: String -> String -> String -> IO()
addUserToListDatabase username creator listName = do
    let listdir = directoryDatabase ++ username ++ "/sharedWithMe"
    let userList = listdir ++ "/" ++ listName
    existDirectory <- doesDirectoryExist listdir
    if not existDirectory then do
        putStrLn "Usuário incorreto, ou não cadastrado."
        putStrLn "Tentar novamente? (s/n)"
        option <- getLine
        (if (option == "s") || (option == "S") then (do
            addUserToListDatabase username creator listName) else return ())
    else do
        existFile <- doesFileExist userList
        if existFile then do
            appendFile userList (creator ++ "\n" ++ listName ++ "\n")
        else do
            withFile userList WriteMode $ \handle -> do
                hPutStrLn handle (creator ++ "\n" ++ listName ++ "\n")

removeUserFromListDatabase :: String -> String -> IO ()
removeUserFromListDatabase username listName = do
    let filePath = directoryDatabase ++ username ++ "/listas/" ++ listName ++ "/users.txt"
    existFile <- doesFileExist filePath
    if existFile then do
        contents <- readFile filePath
        let permissions = lines contents
            newPermissions = filter (/= username) permissions
        writeFile filePath (unlines newPermissions)
    else
        return ()

getSharedListDatabase :: String -> IO [String]
getSharedListDatabase username = do
    let filePath = directoryDatabase ++ username ++ "/sharedWithMe"
    existFile <- doesDirectoryExist filePath
    if existFile then do
        contents <- getDirectoryContents filePath
        let lists = filter (\x -> x /= "." && x /= "..") contents
        return lists
    else
        return []

addTaskDatabase :: String -> String -> String -> String -> String -> String -> IO()
addTaskDatabase username listName taskName taskDesc taskDate taskPriority = do
    let taskcontent = [taskName, taskDesc, taskDate, taskPriority]
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"++taskName
    withFile filePath WriteMode $ \handle -> do
        hPutStr handle (unlines taskcontent)

showTaskContentDatabase :: String -> String -> String -> IO [String]
showTaskContentDatabase username listName taskName = do
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"++taskName
    conteudo <- readFile filePath
    let linhas = lines conteudo
    return linhas

deleteTaskDatabase :: String -> String -> String -> IO()
deleteTaskDatabase username listName taskName = do
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"
    removeFile (filePath ++ taskName)

editTaskDatabase :: String -> String -> String -> String -> String -> IO()
editTaskDatabase username listName taskName newData oldData = do
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"++taskName
    let newFilePath = directoryDatabase++username++"/listas/"++listName++"/"++taskName++"(editado)"
    conteudo <- readFile filePath
    let linhas = lines conteudo
    case oldData of
        "name" -> do
            let newTaskContent = [newData, linhas !! 1, linhas !! 2, linhas !! 3]
            withFile newFilePath WriteMode $ \handle -> do
                hPutStr handle (unlines newTaskContent)
        "description" -> do
            let newTaskContent = [linhas !! 0, newData, linhas !! 2, linhas !! 3]
            withFile newFilePath WriteMode $ \handle -> do
                hPutStr handle (unlines newTaskContent)
        "date" -> do
            let newTaskContent = [linhas !! 0, linhas !! 1, newData, linhas !! 3]
            withFile newFilePath WriteMode $ \handle -> do
                hPutStr handle (unlines newTaskContent)
        "priority" -> do
            let newTaskContent = [linhas !! 0, linhas !! 1, linhas !! 2, newData]
            withFile newFilePath WriteMode $ \handle -> do
                hPutStr handle (unlines newTaskContent)

ifNewTaskExists :: String -> String -> String -> IO ()
ifNewTaskExists username listName taskName = do
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"++taskName++".new"
    existFile <- doesFileExist filePath
    if existFile then do
        removeFile (directoryDatabase++username++"/listas/"++listName++"/"++taskName)
        renameFile filePath (directoryDatabase++username++"/listas/"++listName++"/"++taskName)
    else return ()