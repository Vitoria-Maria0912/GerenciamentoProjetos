:- module(projeto, [lerJSON/2, projetoToJson/7, projetosToJSON/2, salvarProjeto/5, projetoJaExiste/2]). %removerProjetoJSON/3, removerProjeto/2
:- use_module(library(http/json)).


% Lendo arquivo JSON puro
lerJSON(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).



projetoToJson(IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente, Membros, Atividades, Out) :- 
    swritef(Out, '{"idProjeto":"%w","nomeProjeto":"%w","descricaoProjeto":"%w","idGerente":"%w"}', 
    [IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente, [], []]).



% Convertendo uma lista de objetos em JSON 
projetosToJSON([], []).
projetosToJSON([H|T], [P|Projeto]) :- 
		projetoToJSON(H.idProjeto, H.nomeProjeto, H.descricaoProjeto, H.idGerente,  P), 
		projetosToJSON(T, Projeto).


% Salvar em arquivo JSON
salvarProjeto(FilePath, IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente) :- 
		lerJSON(FilePath, File),
		projetosToJSON(File, ListaProjetosJSON),
		projetoToJSON(IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente, Membros, Atividades),
		append(ListaProjetosJSON, [ProjetoJSON], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).


% % Removendo projeto
% removerProjetoJSON([], _, []).
% removerProjetoJSON([H|T], H.idProjeto, T).
% removerProjetoJSON([H|T], Id, [H|Out]) :- removerProjetoJSON(T, IdProjeto, Out).

% removerProjeto(FilePath, IdProjeto) :-
%    lerJSON(FilePath, File),
%    removerProjetoJSON(File, IdProjeto, SaidaParcial),
%    agentesToJSON(SaidaParcial, Saida),
%    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).



% Pega uma atividade por ID
getProjetoJSON(IdProjeto, [Projeto|_], Projeto):- IdProjeto =:= Projeto.IdProjeto.
getProjetoJSON(IdProjeto, [_|T], Projeto):- getProjetoJSON(IdProjeto, T, Projeto).

% Predicado para verificar se uma atividade com o mesmo IdAtividade já existe no sistema
projetoJaExiste(IdProjeto, ProjetosDoSistema) :-
    member(projeto(IdProjeto, _, _, _, _, _, _, _), ProjetosDoSistema).
  