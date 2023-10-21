:- module(projeto, [lerJSON/2, projetoToJSON/7, projetosToJSON/2, salvarProjeto/7, exibirProjetosAux/1,
                    exibirProjetos/1, getProjetoJSON/3, removerProjeto/2, removerProjetoJSON/3, 
                    verifica_id_projeto/3, editarMembros/3, ehGerente/3,usuarioEstaEmProjeto/2, membroDeProjeto/3,membroDeProjeto/2,imprimirProjetos_membro/2,getMembros/2,imprimirProjetos_Gerente/2,addAtividadesProj/3,imprimirProjeto/1]).
:- use_module(library(http/json)).
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").



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
    write('Nome do projeto: '), writeln(H.nomeProjeto),
    write('ID Projeto: '), writeln(H.idProjeto),
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
    writeln('Projeto removido com sucesso. Até a próxima!').

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

addAtividadesProj(FilePath, IdP, NovaAtividade) :-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verifica se um usuário está em um projeto.

% Defina a função getMembros/2.
getMembros(IdProjeto, Membros) :-
    lerJSON('Database/projetos.json', Projetos), % Carrega a lista de projetos do arquivo JSON
    getProjetoJSON(IdProjeto, Projetos, Projeto), % Obtém o projeto com base no IdProjeto
    Membros = Projeto.membros. % Obtém a lista de membros do projeto.


   % Caso base: usuário não é membro de nenhum projeto.
   membroDeProjeto(_, []):- false.
   membroDeProjeto(IdUsuario, [Projeto|OutrosProjetos]) :-
    
    string_para_numero(IdUsuario, Idfake),
    (IdUsuario == Projeto.idGerente -> true;
    member(Idfake, Projeto.membros) ->
        writeln('Projetos em que o usuário é membro: '), imprimirProjeto(Projeto), true;
    membroDeProjeto(IdUsuario, OutrosProjetos)).

    imprimirProjetos_Gerente(_, []).
imprimirProjetos_Gerente(IdUsuario, [Projeto|OutrosProjetos]) :-
    (IdUsuario == Projeto.idGerente) ->
        writeln('Projetos em que o usuário é gerente: '), imprimirProjeto(Projeto),
        imprimirProjetos_Gerente(IdUsuario, OutrosProjetos);
    imprimirProjetos_Gerente(IdUsuario, OutrosProjetos).

imprimirProjetos_membro(_, []).
imprimirProjetos_membro(IdUsuario, [Projeto|OutrosProjetos]) :-
    string_para_numero(IdUsuario, Idfake),
    member(Idfake, Projeto.membros) ->
        writeln('Projetos em que o usuário é membro: '), imprimirProjeto(Projeto), imprimirProjetos_membro(IdUsuario,OutrosProjetos);
    imprimirProjetos_membro(IdUsuario,OutrosProjetos).



   imprimirProjeto(Projeto) :-
    Nome = Projeto.nomeProjeto,
    Id = Projeto.idProjeto,
    write('Título: '), write(Nome),
    write(' (ID: '), write(Id), writeln(')').

% Função que lista, com base no ID, os projetos dos quais o usuário participa.
listarProjetosDoUsuario(IdUsuario, Projetos, ListaProjetos) :-
    % Filtra os projetos nos quais o usuário está participando.
    filtrarProjetosUsuario(IdUsuario, Projetos, ProjetosFiltrados),
    % Formata a lista de IDs dos projetos.
    formatarIdsProjetos(ProjetosFiltrados, ListaProjetos).

% Predicado para filtrar os projetos nos quais o usuário está participando.
filtrarProjetosUsuario(_, [], []).
filtrarProjetosUsuario(IdUsuario, [Projeto|RestoProjetos], ProjetosFiltrados) :-
    % Verifica se o usuário está participando do projeto ou é gerente.
    (usuarioEstaEmProjeto(IdUsuario, Projeto); ehGerente2(IdUsuario, Projeto)),
    % Continua a filtragem dos próximos projetos.
    filtrarProjetosUsuario(IdUsuario, RestoProjetos, ProjetosFiltrados).
filtrarProjetosUsuario(IdUsuario, [_|RestoProjetos], ProjetosFiltrados) :-
    % Ignora o projeto atual e continua a filtragem dos próximos projetos.
    filtrarProjetosUsuario(IdUsuario, RestoProjetos, ProjetosFiltrados).

% Formata uma lista de projetos para uma lista de IDs.
formatarIdsProjetos([], '').
formatarIdsProjetos([Projeto|RestoProjetos], ListaProjetos) :-
    % Extrai o ID do projeto e adiciona-o à lista.
    get_dict(idProjeto, Projeto, Id),
    atomic_list_concat([Id, ' '|Resto], ListaProjetos),
    % Formata o restante dos projetos.
    formatarIdsProjetos(RestoProjetos, Resto).
% Verifica se um usuário é o gerente de um projeto.
ehGerente2(IdUsuario, Projeto) :-
    get_dict(idGerente, Projeto, IdUsuario).
