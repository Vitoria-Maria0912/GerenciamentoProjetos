:- module(projeto, [lerJSON/2, projetoToJSON/7, projetosToJSON/2, salvarProjeto/7, exibirProjetosAux/1,
                    exibirProjetos/1, getProjetoJSON/3, removerProjeto/2, removerProjetoJSON/3, 
                    verifica_id_projeto/3, editarMembros/3, ehGerente/3, membroDeProjeto/3, addAtividadesProjeto/3]).
:- use_module(library(http/json)).
:- use_module("Controllers/Utils.pl").



% Cria um projeto
projetoToJSON(NomeProjeto, DescricaoProjeto, IdProjeto, Atividades, Membros, IdGerente, Projeto) :-
    swritef(Projeto, '{"nomeProjeto":"%w", "descricaoProjeto":"%w", "idProjeto":"%w", "atividadesAtribuidas":%w,"membros":%w, "idGerente":"%w"}', 
    [ NomeProjeto, DescricaoProjeto,IdProjeto, Atividades, Membros, IdGerente]).

% Convertendo uma lista de objetos em JSON para
projetosToJSON([], []).
projetosToJSON([H|T], [P|Projeto]) :-
    projetoToJSON(H.nomeProjeto, H.descricaoProjeto, H.idProjeto, H.atividadesAtribuidas, H.membros, H.idGerente, P),
    projetosToJSON(T, Projeto).

% Salvar em arquivo JSON
salvarProjeto(FilePath, NomeProjeto, DescricaoProjeto, IdProjeto, Atividades, Membros, IdGerente) :-
    lerJSON(FilePath, File),
    projetosToJSON(File, ListaProjetos),
    projetoToJSON(NomeProjeto, DescricaoProjeto, IdProjeto, Atividades, Membros, IdGerente, Projetos),
    append(ListaProjetos, [Projetos], Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe os projetos cadastrados omitindo a descricaoProjeto
exibirProjetosAux([]).
exibirProjetosAux([H|T]) :-
    write('ID Projeto: '), writeln(H.idProjeto),
    write('Nome do projeto: '), writeln(H.nomeProjeto),
    write('Atividades do projeto: '), writeln(H.atividadesAtribuidas),
		nl, exibirProjetosAux(T).

exibirProjetos(FilePath) :-
		lerJSON(FilePath, Projetos),
		exibirProjetosAux(Projetos).




% Pega uma projeto por ID
getProjetoJSON(IdProjeto, [Projeto|_], Projeto):- IdProjeto == Projeto.idProjeto.
getProjetoJSON(IdProjeto, [_|T], Projeto):- getProjetoJSON(IdProjeto, T, Projeto).

% Removendo um usuário - ainda nao funciona
removerProjetoJSON([], _, []).
removerProjetoJSON([H|T], H.idProjeto, T).
removerProjetoJSON([H|T], Id, [H|Out]):- removerProjetoJSON(T, Id, Out).

removerProjeto(FilePath, Id):-
    lerJSON(FilePath, File),
    removerProjetoJSON(File, Id, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream),
    writeln('                                                    '),
    writeln('            |  Projeto removido com sucesso!  |     '),
    writeln('                                                    ').

% verifica se um id existe dentro da lista de projetos
verifica_id_projeto(_, [], false).
verifica_id_projeto(Busca, [Projeto|_], true) :-
    get_dict(idProjeto, Projeto, Id),
    Busca == Id.
verifica_id_projeto(Busca, [_|T], R) :- verifica_id_projeto(Busca, T, R).


% verifica se um id de usuario é de um gerente de projeto
ehGerente(_, [], false).
ehGerente(Busca, [Projeto|_], true) :-
    get_dict(idGerente, Projeto, Id),
    Busca == Id.
ehGerente(Busca, [_|T], R) :- ehGerente(Busca, T, R).

% adiciona atividades a um projeto (ver se funciona na pratica)
editarAtividadesJSON([], _, _, []).
editarAtividadesJSON([H|T], H.idProjeto, NovaAtividade, [_{idProjeto:H.idProjeto, nomeProjeto:H.nomeProjeto, descricaoProjeto:H.descricaoProjeto, atividadesAtribuidas:NovaListaAtividades, membros: H.membros, idGerente:H.idGerente}|T]) :-
    adicionarAtividade(H.atividadesAtribuidas, NovaAtividade, NovaListaAtividades).
editarAtividadesJSON([H|T], Id, NovaAtividade, [H|Out]) :- editarAtividadesJSON(T, Id, NovaAtividade, Out).

adicionarAtividade(ListaAtividades, NovaAtividade, NovaListaAtividades) :-
    NovaListaAtividades = [NovaAtividade|ListaAtividades].

addAtividadesProjeto(FilePath, IdP, NovaAtividade) :-
    lerJSON(FilePath, File),
    editarAtividadesJSON(File, IdP, NovaAtividade, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% adiciona membros a um projeto 
editarMembrosJSON([], _, _, []).
editarMembrosJSON([H|T], H.idProjeto, NovoMembro, [_{idProjeto:H.idProjeto, nomeProjeto:H.nomeProjeto, descricaoProjeto:H.descricaoProjeto, atividadesAtribuidas:H.atividadesAtribuidas, membros:NovaListaDeMembros, idGerente:H.idGerente}|T]) :-
adicionarMembro(H.membros, NovoMembro, NovaListaDeMembros).
editarMembrosJSON([H|T], Id, NovoMembro, [H|Out]) :- editarMembrosJSON(T, Id, NovaAtividade, Out).

adicionarMembro(ListaMembros, NovoMembro, NovaListaDeMembros) :-
NovaListaDeMembros = [NovoMembro|ListaMembros].

editarMembros(FilePath, IdP, NovoMembro) :-
    lerJSON(FilePath, File),
    editarMembrosJSON(File, IdP, NovoMembro, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).


% Predicado para verificar se um usuário é membro de um projeto
membroDeProjeto(IdUsuario, IdProjeto, Projetos) :-
    member(Projeto, Projetos),
    Projeto = [idProjeto=IdProjeto, membros=Membros, idGerente=IdGerente],
    ( member(IdUsuario, Membros) ; IdUsuario = IdGerente ).


% falta adicionar a parte de imprimir membros do projeto e remover membros

