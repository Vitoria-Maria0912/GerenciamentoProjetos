:- module(atividade, [lerBancoDeAtividadesJson/2, atividadeToJSON/5, atividadesToJSON/2, salvarAtividade/5,
                      exibirAtividadesAux/1, exibirAtividades/1, editarIdProjetoAtividadeJSON/4, editarIdProjetoAtividade/3, 
                      editarMembroResponsavelAtividadeJSON/4, getAtividadeJSON/3, atividadeJaExiste/2, removerAtividade/2,verifica_id_atividade/3]).
:- use_module(library(http/json)).

% Lendo arquivo JSON puro
lerBancoDeAtividadesJson(FilePath, File) :-
  open(FilePath, read, F),
  json_read_dict(F, File).

% Cria uma atividade
atividadeToJSON(Titulo, Descricao, Dificuldade, Id_Atividade, Atividade) :-
		swritef(Atividade, '{"titulo":"%w", "descricao":"%w", "dificuldade":"%w", "idAtividade":%w, "status":"%w", "idProjetoAtividade":"%w", "idMembroResponsavel":"%w", "feedbacks":%w}',
    [Titulo, Descricao, Dificuldade, Id_Atividade, 'Não atribuída!', 'Não atribuído!', 'Não atribuído!', []]).

% Convertendo uma lista de objetos em JSON para 
atividadesToJSON([], []).
atividadesToJSON([H|T], [A|Atividade]) :- 
    atividadeToJSON(H.titulo, H.descricao, H.dificuldade, H.idAtividade, A),
    atividadesToJSON(T, Atividade).

% Salvar em arquivo JSON
salvarAtividade(FilePath, Titulo, Descricao, Dificuldade, Id_Atividade) :- 
		lerBancoDeAtividadesJson(FilePath, File),
		atividadesToJSON(File, ListaAtividades),
		atividadeToJSON(Titulo, Descricao, Dificuldade, Id_Atividade, Atividade),
		append(ListaAtividades, [Atividade], Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe as atividade cadastradas 
exibirAtividadesAux([]).
exibirAtividadesAux([H|T]) :- 
    write('Titulo:'), writeln(H.titulo),
		write('Descricao:'), writeln(H.descricao), 
    write('Status:'), writeln(H.status), 
    write('Dificuldade:'), writeln(H.dificuldade), 
    write('ID Projeto:'), writeln(H.status), 
    write('ID Atividade:'), writeln(H.idAtividade), 
    write('ID Membro Responsavel:'), writeln(H.status), 
    write('Feedbacks:'), writeln(H.status), 
		nl, exibirAtividadesAux(T).

exibirAtividades(FilePath) :-
		lerBancoDeAtividadesJson(FilePath, Atividades),
		exibirAtividadesAux(Atividades).

% Verifica se um ID existe dentro da lista de atividades
verifica_id_atividade(_, [], false).
verifica_id_atividade(Busca, [Atividade|_], true) :-
    get_dict(idAtividade, Atividade, Id),
    Busca == Id.
verifica_id_atividade(Busca, [_|T], R) :- verifica_id_atividade(Busca, T, R).

% Predicado para verificar se um ID existe em um banco de dados JSON
verifica_id(IdProcurado, Existe) :-
  % Lê o conteúdo do arquivo JSON em uma estrutura de dados
  lerBancoDeAtividadesJson('Database/bancoDeAtividades.json',Atividades),
  % Verifica se o ID existe na estrutura de dados
  (verifica_id_json(IdProcurado, Atividades) -> Existe = true ; Existe = false).

% Predicado para verificar se um ID existe na estrutura de dados JSON
verifica_id_json(_, []):- false.
verifica_id_json(IdProcurado, [Registro | Resto]) :-
  % Suponhamos que o ID esteja associado à chave "id" no JSON
  % Você pode adaptar isso de acordo com a estrutura do seu JSON
  get_dict(id, Registro, Id),
  (Id = IdProcurado ; verifica_id_json(IdProcurado, Resto)).


% Pega atividades por ID de Projeto
getAtividadesJSON(_, [], []).
getAtividadesJSON(IdProjeto, [Atividade|Resto], AtividadesEncontradas) :-
    Atividade.idProjetoAtividade == IdProjeto,
getAtividadesJSON(IdProjeto, Resto, RestoAtividades),
    AtividadesEncontradas = [Atividade|RestoAtividades].
getAtividadesJSON(IdProjeto, [_|Resto], AtividadesEncontradas) :-
getAtividadesJSON(IdProjeto, Resto, AtividadesEncontradas).

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
  lerBancoDeAtividadesJson(FilePath, File),
  editarStatusAtividadeJSON(File, IdAtividade, Status, SaidaParcial),
  atividadesToJSON(SaidaParcial, Saida),
  open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Pega uma atividade por ID
getAtividadeJSON(IdAtividade, [Atividade|_], Atividade):- IdAtividade =:= Atividade.idAtividade.
getAtividadeJSON(IdAtividade, [_|T], Atividade):- getAtividadeJSON(IdAtividade, T, Atividade).

% Predicado para verificar se uma atividade com o mesmo IdAtividade já existe no sistema
atividadeJaExiste(IdAtividade, AtividadesDoSistema) :-
  member(atividade(IdAtividade, _, _, _, _, _, _, _), AtividadesDoSistema).

% Removendo a atividade pelo ID
removerAtividadeJSON([], _, []).
removerAtividadeJSON([H|T], Id, Out) :-
    H.idAtividade =\= Id,
    removerAtividadeJSON(T, Id, Resto),
    Out = [H | Resto].
removerAtividadeJSON([H|T], Id, Out) :-
    H.idAtividade =:= Id, % Verifica se o ID corresponde
    removerAtividadeJSON(T, Id, Out).

removerAtividade(FilePath, Id) :-
    lerBancoDeAtividadesJson(FilePath, File),
    removerAtividadeJSON(File, Id, SaidaParcial),
    atividadesToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).
