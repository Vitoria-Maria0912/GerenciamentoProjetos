:- use_module(library(http/json)).

% Cria uma atividade
atividadeToJSON(Titulo, Descricao, Status, Dificuldade, Id_Projeto_Atividade, Id_Atividade, Id_Membro_Responsavel, Feedbacks, Atividade) :-
		swritef(Out, '{"titulo":"%w", "descricao":"%w", "status":%w, "dificuldade":%w, "idProjetoAtividade":%w, "idAtividade":%w, "idMembroResponsavel":%w, "feedbacks":%w}',
        [Titulo, Descricao, Status, Dificuldade, Id_Projeto_Atividade, Id_Atividade, Id_Membro_Responsavel, Feedbacks]).

% Convertendo uma lista de objetos em JSON para 
atividadesToJSON([], []).
atividadesToJSON([H|T], [A|Atividade]) :- 
		atividadeToJSON(H.titulo, H.descricao, H.status, H.dificuldade, H.idProjetoAtividade, H.idAtividade, H.idAtividade, H.idMembroResponsavel, H.feedbacks, A), 
		atividadeToJSON(T, Atividade).

% Salvar em arquivo JSON
salvarAtividade(FilePath, Titulo, Descricao, Status, Dificuldade, Id_Projeto_Atividade, Id_Atividade, Id_Membro_Responsavel, Feedbacks) :- 
    id(ID), incrementa_id,
		lerJSON(FilePath, File),
		atividadesToJSON(File, ListaAgentesJSON),
		atividadesToJSON(Nome, Funcao, ID, AgenteJSON),
		append(ListaAgentesJSON, [AgenteJSON], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe as atividade cadastradas 
exibirAtividades(FilePath) :-
		lerJSON(FilePath, Atividades),
		exibirAtividadesAux(Atividades).

% Mudando o nome de uma atividade <<<<<<<<<<<<<<< precisa alterar para que altere só o idProjeto
editarIdProjetoAtividadeJSON([], _, _, []).
editarIdProjetoAtividadeJSON([H|T], H.idAtividade, [_{id_Projeto_Atividade:H.id}|T]).
editarIdProjetoAtividadeJSON([H|T], Id, Nome, [H|Out]) :- 
		editarNomeAgenteJSON(T, Id, Nome, Out).

editarIdProjetoAtividade(FilePath, IdAgente, NovoNome) :-
		lerJSON(FilePath, File),
		editarIdProjetoAtividadeJSON(File, IdAgente, NovoNome, SaidaParcial),
		atividadesToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Removendo agente
removerAtividade([], _, []).
removerAtividadeJSON([H|T], H.id, T).
removerAtividadeJSON([H|T], Id, [A|Atividade]) :- removerAtividadeJSON(T, Id, Atividade).

removerAtividade(FilePath, Id) :-
   lerJSON(FilePath, File),
   removerAtividadeJSON(File, Id, SaidaParcial),
   atividadesToJSON(SaidaParcial, Saida),
   open(FilePath, write, Stream), write(Stream, Saida), close(Stream).
   
% Lendo arquivo JSON puro
lerJSON(FilePath, File) :-
		open(FilePath, read, F),
		json_read_dict(F, File).


% ---------------------------------- Haskell ------------------------------------------------------------


-- Cria uma atividade
criarAtividade :: String -> String -> String -> Maybe Int -> Int -> Maybe Int -> [String] -> IO()
criarAtividade   filePath titulo descricao idProjetoAtividade idAtividade idMembroResponsavel feedback = do
  let atividade = Atividade titulo descricao "Não atribuída!" idProjetoAtividade idAtividade idMembroResponsavel feedback
  escreverAtividade filePath atividade

-- Adiciona atividades à bancoDeAtividades.json
escreverAtividade :: String -> Atividade -> IO()
escreverAtividade filePath atividade = do

  let listaAtividades = getTodasAtividades filePath ++ [atividade]

  B.writeFile "../Temp.json" $ encode listaAtividades
  removeFile filePath
  renameFile "../Temp.json" filePath

-- Remove uma atividade da lista de atividades
apagarAtividade :: Int -> [Atividade] -> [Atividade]
apagarAtividade _ [] = []
apagarAtividade atividadeId (x:xs)
  | idAtividade x == atividadeId = xs
  | otherwise = x : apagarAtividade atividadeId xs

-- Remove uma atividade do arquivo.JSON
deletarAtividade :: String -> Int -> IO()
deletarAtividade filePath idAtividade = do
    let atividades = getTodasAtividades filePath
    let atividadesAtualizadas = apagarAtividade idAtividade atividades

    B.writeFile "../Temp.json" $ encode atividadesAtualizadas
    removeFile filePath
    renameFile "../Temp.json" filePath

-- | Funções que lidam com o status de uma atividade | --

-- | Função que muda o status de uma atividade
mudaStatus :: Int -> [Atividade] -> String -> [Atividade]
mudaStatus _ [] _ = []
mudaStatus id (ativ:ativs) novoStatus
  | idAtividade ativ == id = ativ {status = novoStatus} : mudaStatus id ativs novoStatus
  | otherwise = ativ : mudaStatus id ativs novoStatus

-- | Função que edita o status sobreescrevendo o arquivo
editStatus :: String ->  Int ->  String -> IO()
editStatus jsonFilePath idAtividade novoStatus = do
  let listaAtividades = getTodasAtividades jsonFilePath
  let atividadesAtualizadas = mudaStatus idAtividade listaAtividades novoStatus

  B.writeFile "../Temp.json" $ encode atividadesAtualizadas
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

-- | Função que obtém o status da atividade
getStatus :: Atividade -> String
getStatus atividade = status atividade

-- | Função que pega o ID do membro responsável pela atividade
getMembroResponsavel :: Atividade -> String
getMembroResponsavel atividade = do
    let membroResponsavel = idMembroResponsavel atividade
    case membroResponsavel of
        Just _ -> show (idMembroResponsavel atividade)
        _ -> "Não atribuído!"

-- | Função que verifica se um usuário (através do ID) é responsável por alguma atividade
ehMembroResponsavel :: Maybe Int -> Atividade -> Bool
ehMembroResponsavel membroResponsavelId atividade = membroResponsavelId == (idMembroResponsavel atividade)
    
-- | Funções que lidam com os feedbacks de uma atividade | --

-- | Função que adiciona um feedback a lista de feedbacks de uma atividade
addFeedback :: Int -> [Atividade] -> String -> [Atividade]
addFeedback _ [] _ = []
addFeedback id (ativ:ativs) novoFeedback
  | idAtividade ativ == id = ativ { feedbacks = feedbacks ativ ++ [novoFeedback] } : addFeedback id ativs novoFeedback
  | otherwise = ativ : addFeedback id ativs novoFeedback

-- | Função que remove todos os feedbacks da lista de feedbacks
removeFeedback :: Int -> [Atividade] -> [Atividade]
removeFeedback _ [] = []
removeFeedback id (ativ:ativs)
  | idAtividade ativ == id = ativ {feedbacks = []} : removeFeedback id ativs
  | otherwise = ativ : removeFeedback id ativs

-- | Função que edita o arquivo ao adicionar ou remover um projeto
editFeedback :: String ->  Int ->  String -> Bool -> IO()
editFeedback jsonFilePath idAtividade feedback adicionar = do
  let listaAtividades = getTodasAtividades jsonFilePath
  let atividadesAtualizadas = if adicionar
                              then addFeedback idAtividade listaAtividades feedback
                              else removeFeedback idAtividade listaAtividades

  B.writeFile "../Temp.json" $ encode atividadesAtualizadas
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

-- | Função que obtém os feedbacks da atividade
getFeedbacks :: Atividade -> [String]
getFeedbacks atividade = feedbacks atividade

-- | Funções que lidam com o id de um projeto relacionado a uma atividade | --

-- | Função que adiciona o id de um projeto a uma atividade
addIdProj :: Int -> [Atividade] -> Int -> [Atividade]
addIdProj _ [] _ = []
addIdProj id (ativ: ativs) novoId
  | idAtividade ativ == id = ativ {idProjetoAtividade = Just novoId} : addIdProj id ativs novoId
  | otherwise = ativ : addIdProj id ativs novoId

-- | Função que remove o id de um projeto de uma atividade
removeIdProj :: Int -> [Atividade] -> [Atividade]
removeIdProj _ [] = []
removeIdProj id (ativ:ativs)
  | idAtividade ativ == id = ativ {idProjetoAtividade = Nothing} : removeIdProj id ativs
  | otherwise = ativ : removeIdProj id ativs

-- | Função que edita o arquivo ao adicionar ou remover o id de um projeto de uma atividade
editIdProj :: String ->  Int ->  Int -> Bool -> IO()
editIdProj jsonFilePath idAtividade idProjeto adicionar = do
  let listaAtividades = getTodasAtividades jsonFilePath
  let atividadesAtualizadas = if adicionar 
                              then addIdProj idAtividade listaAtividades idProjeto
                              else removeIdProj idAtividade listaAtividades 
  B.writeFile "../Temp.json" $ encode atividadesAtualizadas
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath

-- | Função que checa se uma atividade existe e se está relacionada a um projeto
temIdProjeto :: Int -> [Atividade] -> Bool
temIdProjeto id atividades =
  case getAtividade id atividades of
    Just atividade -> isJust (idProjetoAtividade atividade)
    Nothing -> False
  where
    isJust (Just _) = True
    isJust Nothing = False

-- | Funções que lidam com o membro responsável de uma atividade | --  

-- | Função que adiciona um membro como responsável
addMembroResp :: Int -> [Atividade] -> Int -> [Atividade]
addMembroResp _ [] _ = []
addMembroResp id (ativ:ativs) idMembro
  | idAtividade ativ == id = ativ {idMembroResponsavel = Just idMembro} : addMembroResp id ativs idMembro
  | otherwise = ativ : addMembroResp id ativs idMembro

-- | Função que remove um membro como responsável
removeIdMembroResp :: Int -> [Atividade] -> [Atividade]
removeIdMembroResp _ [] = []
removeIdMembroResp id (ativ:ativs)
  | idAtividade ativ == id = ativ {idMembroResponsavel = Nothing} : removeIdMembroResp id ativs
  | otherwise = ativ : removeIdMembroResp id ativs

-- | Função que edita o membro no arquivo ao adicionar ou removê-lo
editMembroResp :: String ->  Int ->  Int -> Bool -> IO()
editMembroResp jsonFilePath idAtividade idMembro adicionar = do
  let listaAtividades = getTodasAtividades jsonFilePath
  let atividadesAtualizadas = if adicionar 
                              then addMembroResp idAtividade listaAtividades idMembro
                              else removeIdMembroResp idAtividade listaAtividades 
  B.writeFile "../Temp.json" $ encode atividadesAtualizadas
  removeFile jsonFilePath
  renameFile "../Temp.json" jsonFilePath



-- | Função que obtém uma atividade a partir do ID
getAtividade :: Int -> [Atividade] -> Maybe Atividade
getAtividade _ [] = Nothing
getAtividade atividadeId (x:xs)
  | idAtividade x == atividadeId = Just x
  | otherwise = getAtividade atividadeId xs



-- | Função que obtém as todas atividades cadastradas no sistema
getTodasAtividades :: String -> [Atividade]
getTodasAtividades filePath = do
    let arquivo = unsafePerformIO (B.readFile filePath)
    let decodedFile = decode arquivo :: Maybe [Atividade]
    case decodedFile of
        Nothing -> []
        Just out -> out

-- | Função que imprime as atividades para visualização
imprimirAtividade :: Atividade -> IO()
imprimirAtividade atividade = 
  if (idProjetoAtividade atividade == Nothing)then do
  putStrLn $  "             Título: " ++ (titulo atividade) ++ "\n" ++
              "             Descrição: " ++ (descricao atividade) ++ "\n" ++
              "             ID Projeto: " ++ " - " ++ "\n" ++
              "             ID Atividade: " ++ show (idAtividade atividade) ++ "\n" ++
              "             Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
              "             Status: " ++ status atividade ++ "\n"
  else do
    let getIdProjeto = removerTodasAsPalavras "Just" (show (idProjetoAtividade atividade))
    putStrLn $ "             Título: " ++ (titulo atividade) ++ "\n" ++
               "             Descrição: " ++ (descricao atividade) ++ "\n" ++
               "             ID Projeto: " ++ getIdProjeto ++ "\n" ++             
               "             ID Atividade: " ++ show (idAtividade atividade) ++ "\n" ++
               "             Membro Responsável: " ++ (getMembroResponsavel atividade) ++ "\n" ++
               "             Status: " ++ status atividade ++ "\n"

-- Função para remover todas as ocorrências de uma palavra de uma string
removerTodasAsPalavras :: String -> String -> String
removerTodasAsPalavras palavra string =
    let palavraText = pack palavra
        stringText = pack string
        resultadoText = replace palavraText "" stringText
    in
    unpack resultadoText


