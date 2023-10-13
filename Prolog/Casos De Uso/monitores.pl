:- use_module(library(http/json)).

% Fato dinâmico para gerar o id dos agentes
id(1).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

% Lendo arquivo JSON puro
lerJSON(FilePath, File) :-
		open(FilePath, read, F),
		json_read_dict(F, File).

% Regras para listar todos agentes
exibirAgentesAux([]).
exibirAgentesAux([H|T]) :- 
    write("ID:"), writeln(H.id),
		write("Nome:"), writeln(H.nome), 
		write("Funcao:"), writeln(H.funcao), nl, exibirAgentesAux(T).

exibirAgentes(FilePath) :-
		lerJSON(FilePath, Disciplinas),
		exibirAgentesAux(Disciplinas).

% Criando representação em formato String de um agente em JSON
agenteToJSON(Nome, Funcao, ID, Out) :-
		swritef(Out, '{"nome":"%w","funcao":"%w","id":%w}', [Nome, Funcao, ID]).

% Convertendo uma lista de objetos em JSON para 
agentesToJSON([], []).
agentesToJSON([H|T], [X|Out]) :- 
		agenteToJSON(H.nome, H.funcao, H.id, X), 
		agentesToJSON(T, Out).

% Salvar em arquivo JSON
salvarAgente(FilePath, Nome, Funcao) :- 
    id(ID), incrementa_id,
		lerJSON(FilePath, File),
		agentesToJSON(File, ListaAgentesJSON),
		agenteToJSON(Nome, Funcao, ID, AgenteJSON),
		append(ListaAgentesJSON, [AgenteJSON], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Mudando o nome de um agente
editarNomeAgenteJSON([], _, _, []).
editarNomeAgenteJSON([H|T], H.id, Nome, [_{id:H.id, nome:Nome, funcao:H.funcao}|T]).
editarNomeAgenteJSON([H|T], Id, Nome, [H|Out]) :- 
		editarNomeAgenteJSON(T, Id, Nome, Out).

editarNomeAgente(FilePath, IdAgente, NovoNome) :-
		lerJSON(FilePath, File),
		editarNomeAgenteJSON(File, IdAgente, NovoNome, SaidaParcial),
		agentesToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Removendo agente
removerAgenteJSON([], _, []).
removerAgenteJSON([H|T], H.id, T).
removerAgenteJSON([H|T], Id, [H|Out]) :- removerAgenteJSON(T, Id, Out).

removerAgente(FilePath, Id) :-
   lerJSON(FilePath, File),
   removerAgenteJSON(File, Id, SaidaParcial),
   agentesToJSON(SaidaParcial, Saida),
   open(FilePath, write, Stream), write(Stream, Saida), close(Stream).