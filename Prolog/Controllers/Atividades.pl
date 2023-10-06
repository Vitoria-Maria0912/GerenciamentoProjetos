:- module(atividade, [lerBancoDeAtividadesJson/2, atividadeToJSON/8, atividadesToJSON/2, salvarAtividade/5,
                      exibirAtividadesAux/1, exibirAtividades/1, editarIdProjetoAtividadeJSON/4, 
                      editarMembroResponsavelAtividadeJSON/4, getAtividadeJSON/3, atividadeJaExiste/2]).
:- use_module(library(http/json)).

% Lendo arquivo JSON puro
lerBancoDeAtividadesJson(FilePath, File) :-
  open(FilePath, read, F),
  json_read_dict(F, File).

% Cria uma atividade
atividadeToJSON(Titulo, Descricao, _ , Dificuldade, _ , Id_Atividade, _ , Atividade) :-
		swritef(Atividade, '{"titulo":"%w", "descricao":"%w", "status":"%w", "dificuldade":"%w", "idProjetoAtividade":"%w", "idAtividade":%w, "idMembroResponsavel":"%w", "feedbacks":%w}',
    [Titulo, Descricao, 'Não atribuída!', Dificuldade, ' ------- ', Id_Atividade, ' ------- ', []]).

% Convertendo uma lista de objetos em JSON para 
atividadesToJSON([], []).
atividadesToJSON([H|T], [A|Atividade]) :- 
    atividadeToJSON(H.titulo, H.descricao, H.status, H.dificuldade, H.idProjetoAtividade, H.idAtividade, H.idMembroResponsavel, A),
    atividadesToJSON(T, Atividade).

% Salvar em arquivo JSON
salvarAtividade(FilePath, Titulo, Descricao, Dificuldade, Id_Atividade) :- 
		lerBancoDeAtividadesJson(FilePath, File),
		atividadesToJSON(File, ListaAtividades),
		atividadeToJSON(Titulo, Descricao, _ , Dificuldade, _ , Id_Atividade, _ , Atividades),
		append(ListaAtividades, [Atividades], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe as atividade cadastradas 
exibirAtividadesAux([]).
exibirAtividadesAux([H|T]) :- 
    write('Titulo:'), writeln(H.titulo),
		write('Descricao:'), writeln(H.descricao), 
    write('Status:'), writeln(H.status), 
    write('Dificuldade:'), writeln(H.descricao), 
    write('ID Projeto:'), writeln(H.descricao), 
    write('ID Atividade:'), writeln(H.descricao), 
    write('ID Membro Responsavel:'), writeln(H.descricao), 
    write('Feedbacks:'), writeln(H.descricao), 
		nl, exibirAtividadesAux(T).

exibirAtividades(FilePath) :-
		lerBancoDeAtividadesJson(FilePath, Atividades),
		exibirAtividadesAux(Atividades).

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
		lerBancoDeAtividadesJson(FilePath, File),
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
		lerBancoDeAtividadesJson(FilePath, File),
		editarMembroResponsavelAtividadeJSON(File, IdAtividade, IdMembroResponsavel, SaidaParcial),
		atividadesToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream), !.

% Pega uma atividade por ID
getAtividadeJSON(IdAtividade, [Atividade|_], Atividade):- IdAtividade =:= Atividade.idAtividade.
getAtividadeJSON(IdAtividade, [_|T], Atividade):- getAtividadeJSON(IdAtividade, T, Atividade).

% Predicado para verificar se uma atividade com o mesmo IdAtividade já existe no sistema
atividadeJaExiste(IdAtividade, AtividadesDoSistema) :-
  member(atividade(IdAtividade, _, _, _, _, _, _, _), AtividadesDoSistema).
