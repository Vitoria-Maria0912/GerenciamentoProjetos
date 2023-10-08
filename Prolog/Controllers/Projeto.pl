:- module(projeto, [lerJSON/2, projetoToJSON/7, projetosToJSON/2, salvarProjeto/5,
                    removerProjetoJSON/3, removerProjeto/2]).
:- use_module(library(http/json)).


% Lendo arquivo JSON puro
lerJSON(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).


% Fato dinâmico para gerar o id dos projetos
id(1).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

 % Criando representação em formato String de um projeto em JSON
projetoToJson(IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente, Membros, Atividades, Out) :- 
    swritef(Out, '{"idProjeto":%w,"nomeProjeto":%w,"descricaoProjeto":%w,"idGerente":%w,"membros":%w,"atividades":%w}', 
    [IdProjeto, NomeProjeto, DescricaoProjeto, IdGerente, Membros, Atividades]).

% Convertendo uma lista de objetos em JSON 
projetosToJSON([], []).
projetosToJSON([H|T], [P|Projeto]) :- 
		projetoToJSON(H.idProjeto, H.nomeProjeto, H.descricaoProjeto, H.idGerente , H.membros , H.atividades ,  P), 
		projetosToJSON(T, Projeto).


% Salvar em arquivo JSON
salvarProjeto(FilePath, NomeProjeto, DescricaoProjeto, Membros, Atividades) :- 
        idProjeto(IdProjeto), incrementa_id,
        idGerente(IdGerente), incrementa_id,
		lerJSON(FilePath, File),
		projetosToJSON(File, ListaProjetosJSON),
		projetoToJSON(Nome, Funcao, ID, ProjetoJSON),
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
  