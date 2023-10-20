:- module(atividade, [lerJSON/2, atividadeToJSON/9, atividadesToJSON/2, salvarAtividade/9,
                      exibirAtividadesAux/1, exibirAtividades/1, editarIdProjetoAtividadeJSON/4, editarIdProjetoAtividade/3, 
                      editarMembroResponsavelAtividadeJSON/4, getAtividadesJSON/3, atividadeJaExiste/3, removerAtividade/2,
                      verifica_id_atividade/3, getAtividadeJSON/3, editarStatusAtividade/3, editarStatusAtividadeJSON/4,
                      exibirAtividade/1, exibirAtividades/1, getMembroResponsavel/2, editarFeedbacks/4, criarFeedback/3]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module("Controllers/Utils.pl").

% Cria uma atividade
atividadeToJSON(Titulo, Descricao, Dificuldade, Id_Atividade, Status, IdProjetoAtividade, IdMembroResponsavel, Feedbacks, Atividade) :-
		swritef(Atividade, '{"titulo":"%w", "descricao":"%w", "dificuldade":"%w", "idAtividade":"%w", "status":"%w", "idProjetoAtividade":"%w", "idMembroResponsavel":"%w", "feedbacks":"%w"}',
    [Titulo, Descricao, Dificuldade, Id_Atividade, Status, IdProjetoAtividade, IdMembroResponsavel, Feedbacks]).

% Convertendo uma lista de objetos em JSON para 
atividadesToJSON([], []).
atividadesToJSON([H|T], [A|Atividade]) :- 
    atividadeToJSON(H.titulo, H.descricao, H.dificuldade, H.idAtividade, H.status, H.idProjetoAtividade, H.idMembroResponsavel, H.feedbacks, A),
    atividadesToJSON(T, Atividade).

% Salvar em arquivo JSON
salvarAtividade(FilePath, Titulo, Descricao, Dificuldade, Id_Atividade, Status, IdProjetoAtividade, IdMembroResponsavel, Feedbacks) :-
		lerJSON(FilePath, File),
    atividadesToJSON(File, ListaAtividades),
		atividadeToJSON(Titulo, Descricao, Dificuldade, Id_Atividade, Status, IdProjetoAtividade, IdMembroResponsavel, Feedbacks, Atividade),
		append(ListaAtividades, [Atividade], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe as atividade cadastradas 
exibirAtividadesAux([]).
exibirAtividadesAux([H|T]) :- 
    write('|- ID Atividade: '), writeln(H.idAtividade), 
    write('|- Titulo: '), writeln(H.titulo),
		write('|- Descricao: '), writeln(H.descricao), 
    write('|- Status: '), writeln(H.status), 
    write('|- Dificuldade: '), writeln(H.dificuldade), 
    write('|- ID Projeto: '), writeln(H.idProjetoAtividade), 
    write('|- ID Membro Responsavel: '), writeln(H.idMembroResponsavel), 
    write('|- Feedbacks: '), writeln(H.feedbacks), 
		nl, exibirAtividadesAux(T).

exibirAtividades(FilePath) :-
		lerJSON(FilePath, Atividades),
		exibirAtividadesAux(Atividades).

% Exibe as atividade cadastradas 
exibirAtividade(Atividade) :- 
    write('|- ID Atividade: '), writeln(Atividade.idAtividade), 
    write('|- Titulo: '), writeln(Atividade.titulo),
    write('|- Descricao: '), writeln(Atividade.descricao), 
    write('|- Dificuldade: '), writeln(Atividade.dificuldade), 
    write('|- Status: '), writeln(Atividade.status), 
    write('|- Feedbacks: '), writeln(Atividade.feedbacks),
		nl.

% Verifica se um ID existe dentro da lista de atividades
verifica_id_atividade(_, [], false).
verifica_id_atividade(Busca, [Atividade|_], true) :-
    get_dict(idAtividade, Atividade, Id),
    Busca == Id.
verifica_id_atividade(Busca, [_|T], R) :- verifica_id_atividade(Busca, T, R).

% Verifica se um ID é de um membro responsavel
verifica_id_membroResp(_, [], false).
verifica_id_membroResp(Busca, [Atividade|_], true) :-
    get_dict(idMembroResponsavel, Atividade, Id),
    Busca == Id.
verifica_id_membroResp(Busca, [_|T], R) :- verifica_id_membroResp(Busca, T, R).

% Pega atividades por ID de Projeto
getAtividadesJSON(_, [], []).
getAtividadesJSON(IdProjeto, [Atividade|Resto], AtividadesEncontradas) :-
    Atividade.idProjetoAtividade == IdProjeto,
getAtividadesJSON(IdProjeto, Resto, RestoAtividades),
    AtividadesEncontradas = [Atividade|RestoAtividades].
getAtividadesJSON(IdProjeto, [_|Resto], AtividadesEncontradas) :-
getAtividadesJSON(IdProjeto, Resto, AtividadesEncontradas).

% pega atividade por ID
getAtividadeJSON(IdAtividade, [Atividade|_], Atividade):- IdAtividade == Atividade.idAtividade.
getAtividadeJSON(IdAtividade, [_|T], Atividade):- getAtividadeJSON(IdAtividade, T, Atividade). 

% Muda o idProjeto de uma atividade
editarIdProjetoAtividadeJSON([], _, _, []).
editarIdProjetoAtividadeJSON([H|T], H.idAtividade, IdProjetoAtividade, [_{titulo:H.titulo, 
                                                                         descricao:H.descricao,
                                                                         status:H.status, 
                                                                         dificuldade:H.dificuldade, 
                                                                         idProjetoAtividade:IdProjetoAtividade, 
                                                                         idAtividade:H.idAtividade, 
                                                                         idMembroResponsavel:H.idMembroResponsavel, 
                                                                         feedbacks:H.feedbacks}|T]).

editarIdProjetoAtividadeJSON([H|T], IdAtividade, IdProjetoAtividade, [H|Out]) :- editarIdProjetoAtividadeJSON(T, IdAtividade, IdProjetoAtividade, Out).

editarIdProjetoAtividade(FilePath, IdAtividade, IdProjetoAtividade) :-
    lerJSON(FilePath, File),
		editarIdProjetoAtividadeJSON(File, IdAtividade, IdProjetoAtividade, SaidaParcial),
		atividadesToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream), !.

% Muda o membroResponsavel de uma atividade
editarMembroResponsavelAtividadeJSON([], _, _, []).
editarMembroResponsavelAtividadeJSON([H|T], H.idAtividade, IdMembroResponsavel, [_{titulo:H.titulo, 
                                                                         descricao:H.descricao,
                                                                         status:H.status, 
                                                                         dificuldade:H.dificuldade, 
                                                                         idProjetoAtividade:H.idProjetoAtividade, 
                                                                         idAtividade:H.idAtividade, 
                                                                         idMembroResponsavel:IdMembroResponsavel, 
                                                                         feedbacks:H.feedbacks}|T]).

editarMembroResponsavelAtividadeJSON([H|T], IdAtividade, IdMembroResponsavel, [H|Out]) :- editarMembroResponsavelAtividadeJSON(T, IdAtividade, IdMembroResponsavel, Out).

editarMembroResponsavelAtividadeJSON(FilePath, IdAtividade, IdMembroResponsavel) :-
    lerJSON(FilePath, File),
		editarMembroResponsavelAtividadeJSON(File, IdAtividade, IdMembroResponsavel, SaidaParcial),
		atividadesToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream), !.

getMembroResponsavel(Atividade, MembroResponsavel) :- MembroResponsavel = Atividade.idMembroResponsavel.

% Muda o status da Atividade
editarStatusAtividadeJSON([], _, _, []).
editarStatusAtividadeJSON([H|T], H.idAtividade, NovoStatus, [_{ titulo:H.titulo, 
                                                            descricao:H.descricao,
                                                            status:NovoStatus, 
                                                            dificuldade:H.dificuldade, 
                                                            idProjetoAtividade:H.idProjetoAtividade, 
                                                            idAtividade:H.idAtividade, 
                                                            idMembroResponsavel:H.idMembroResponsavel, 
                                                            feedbacks:H.feedbacks}|T]).

editarStatusAtividadeJSON([H|T], IdAtividade, Status, [H|Out]) :- editarStatusAtividadeJSON(T, IdAtividade, Status, Out).

editarStatusAtividade(FilePath, IdAtividade, Status) :-
  lerJSON(FilePath, File),
  editarStatusAtividadeJSON(File, IdAtividade, Status, SaidaParcial),
  atividadesToJSON(SaidaParcial, Saida),
  open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Predicado para verificar se uma atividade com o mesmo IdAtividade já existe no sistema
atividadeJaExiste(_, [], false). 
atividadeJaExiste(Busca, [Atividade|_], true) :- 
    get_dict(idAtividade, Atividade, Id),
    Busca == Id.
atividadeJaExiste(Busca, [_|T], R) :- atividadeJaExiste(Busca, T, R).

% Adiciona um feedback a uma atividade
criarFeedback(FilePath, Atividade, NovoFeedback) :-

  lerJSON(FilePath, Atividades),  
  AtividadeAtualizada = Atividade.put(feedbacks, [NovoFeedback|Atividade.feedbacks]),
  selectchk(Atividade, Atividades, AtividadeAtualizada, AtividadesAtualizadas),
  open(FilePath, write, Stream),
  json_write(Stream, AtividadesAtualizadas, [width(0)]), 
  close(Stream).
