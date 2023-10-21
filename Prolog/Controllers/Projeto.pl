:- module(projeto, [lerJSON/2, projetoToJSON/7, projetosToJSON/2, salvarProjeto/7, exibirProjetosAux/1,
                    exibirProjetos/1, getProjetoJSON/3, removerProjeto/2, removerProjetoJSON/3, 
                    verifica_id_projeto/3, editarMembros/3, ehGerente/3, membroDoProjeto/2, ehMembro/2, 
                    addAtividadesProj/3, retornarMembros/2, exibirMembros/3, exibirAtividadesDoProjeto/3,
                    retornarAtividadesDoProjeto/2]).

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

% adiciona atividades a um projeto 
editarAtividadesJSON([], _, _, []).
editarAtividadesJSON([H|T], H.idProjeto, NovaAtividade, [NovoProjeto|T]) :-
    append(H.atividadesAtribuidas, [NovaAtividade], NovaListaAtividades),
    NovoProjeto = _{
        idProjeto:H.idProjeto,
        nomeProjeto:H.nomeProjeto,
        descricaoProjeto:H.descricaoProjeto,
        atividadesAtribuidas:NovaListaAtividades,
        membros:H.membros,
        idGerente:H.idGerente
    }.
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
editarMembrosJSON([H|T], H.idProjeto, NovoMembro, [NovoProjeto|T]) :-
    append(H.membros, [NovoMembro], NovaListaDeMembros),
    NovoProjeto = _{
        idProjeto:H.idProjeto,
        nomeProjeto:H.nomeProjeto,
        descricaoProjeto:H.descricaoProjeto,
        atividadesAtribuidas:H.atividadesAtribuidas,
        membros:NovaListaDeMembros,
        idGerente:H.idGerente
    }.
editarMembrosJSON([H|T], Id, NovoMembro, [H|Out]) :- editarMembrosJSON(T, Id, NovoMembro, Out).

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

exibirMembros(IdProjeto, Projetos, ListaMembros) :-
    getProjetoJSON(IdProjeto, Projetos, Projeto),
    ListaMembros = Projeto.membros,
    retornarMembros(ListaMembros, Usuarios).

% Checa se o usuário é membro de algum projeto.
% Caso base: usuário não é membro de nenhum projeto.
ehMembro(_, []):- false.
% Caso em que o usuário é membro de algum projeto.
ehMembro(IdUsuario, [Projeto|OutrosProjetos]) :-
     Membros = (Projeto.membros),
     string_para_numero(IdUsuario, Idfake),
    % Verifica se o usuário é o gerente do projeto ou é membro do projeto.
    (Projeto.idGerente == IdUsuario; 
         member(Idfake, Projeto.membros)) -> true;
    % Caso o usuário não seja o gerente nem membro do projeto, verifica nos outros projetos.
ehMembro(IdUsuario, OutrosProjetos).

% Checa se o usuário é membro de um projeto específico
membroDoProjeto(_, Projeto):- false.
membroDoProjeto(IdUsuario, Projeto) :-
     Membros = (Projeto.membros),
     string_para_numero(IdUsuario, Idfake),
    (Projeto.idGerente == IdUsuario; 
         member(Idfake, Projeto.membros)) -> true.

exibirMembros(IdProjeto, Projetos, Usuarios) :-
    getProjetoJSON(IdProjeto, Projetos, Projeto),
    ListaMembros = Projeto.membros,
    retornarMembros(ListaMembros, Usuarios).
    
retornarMembros([], _).
retornarMembros([IdMembro|T], Usuarios) :-
    atom_string(IdMembro, StringId),
    getUsuarioJSON(StringId, Usuarios, Usuario),
    exibirUsuario(Usuario),
    retornarMembros(T, Usuarios).

exibirAtividadesDoProjeto(IdProjeto, Projetos, Atividades) :-
    getProjetoJSON(IdProjeto, Projetos, Projeto),
    ListaDeAtividades = Projeto.atividadesAtribuidas,
    retornarAtividadesDoProjeto(ListaDeAtividades, Atividades).
        
retornarAtividadesDoProjeto([], _).
retornarAtividadesDoProjeto([IdAtividadesDoProjeto|T], Atividades) :-
    atom_string(IdAtividadesDoProjeto, StringId),
    getAtividadeJSON(IdAtividadesDoProjeto, Atividades, Atividade),
    exibirAtividade(Atividade),
    retornarAtividadesDoProjeto(T, Atividades).


