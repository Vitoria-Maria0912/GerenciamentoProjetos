:- module(projeto, [lerJSON/2, projetoToJSON/7, projetosToJSON/2, salvarProjeto/7, exibirProjetosAux/1,
                    exibirProjetos/1, getProjetoJSON/3, removerProjeto/2, removerProjetoJSON/3, 
                    verifica_id_projeto/3, editarMembros/3, ehGerente/3, membroDoProjeto/2, ehMembro/2, jaAtribuida/2, addAtividadesProj/3, retornarMembros/4, exibirMembros/2, removerMembro/3]).
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

addAtividadesProj(FilePath, IdP, NovaAtividade) :-
    lerJSON(FilePath, File),
    editarAtividadesJSON(File, IdP, NovaAtividade, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% adiciona os membros ao JSON
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

% adiciona membros a um projeto 
editarMembros(FilePath, IdP, NovoMembro) :-
    lerJSON(FilePath, File),
    editarMembrosJSON(File, IdP, NovoMembro, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).


% Checa se o usuário é membro de algum projeto.
ehMembro(_, []):- false.
ehMembro(IdUsuario, [Projeto|OutrosProjetos]) :-
     Membros = (Projeto.membros),
     string_para_numero(IdUsuario, Idfake),
    (Projeto.idGerente == IdUsuario; 
         member(Idfake, Projeto.membros)) -> true;
ehMembro(IdUsuario, OutrosProjetos).

% Checa se o usuário é membro de um projeto específico
membroDoProjeto(_, Projeto):- false.
membroDoProjeto(IdUsuario, Projeto) :-
     Membros = (Projeto.membros),
     string_para_numero(IdUsuario, Idfake),
    (Projeto.idGerente == IdUsuario; 
         member(Idfake, Projeto.membros)) -> true.

% % Retorna os membros de um projeto de acordo com o seu ID.
% retornarMembros(IdProjeto, Projetos, ListaMembros, Usuarios) :-
%      %findall(Membro, membroDeProjeto(_, IdProjeto, Projetos), ListaMembros).
%     getProjetoJSON(IdProjeto, Projetos, Projeto),
%     ListaMembros = Projeto.membros,
%     exibirMembros(ListaMembros, Usuarios).
   

% % Exibe os membros da lista de membros do projeto
% exibirMembros([], _).
% exibirMembros([IdMembro|T], Usuarios) :-
% atom_string(IdMembro, StringId),
% getUsuarioJSON(StringId, Usuarios, Usuario),
% exibirUsuario(Usuario),
% exibirMembros(T, Usuarios).


%testandooo-----------------------------
% Exibe os membros do projeto
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
%------------------------







% Checa se uma atividade já está atribuida ao projeto
jaAtribuida(_, Projeto):- false.
jaAtribuida(IdAtividade, Projeto) :-
     Atividades = (Projeto.atividadesAtribuidas),
     string_para_numero(IdAtividade, Idfake),
         member(Idfake, Projeto.atividadesAtribuidas) -> true.


% remove um membro de um projeto 
removerMembroJSON([], _, _, []).
removerMembroJSON([H|T], IdProjeto, IdMembro, [NovoProjeto|T]) :-
    ( H.idProjeto \= IdProjeto -> NovoProjeto = H ;
      delete(H.membros, IdMembro, NovaListaDeMembros),
      NovoProjeto = _{
        idProjeto:H.idProjeto,
        nomeProjeto:H.nomeProjeto,
        descricaoProjeto:H.descricaoProjeto,
        atividadesAtribuidas:H.atividadesAtribuidas,
        membros:NovaListaDeMembros,
        idGerente:H.idGerente
      }
    ).
removerMembroJSON([H|T], Id, IdMembro, [H|Out]) :- removerMembroJSON(T, Id, IdMembro, Out).

% remove um membro de um projeto 
removerMembro(FilePath, IdP, IdMembro) :-
    lerJSON(FilePath, File),
    removerMembroJSON(File, IdP, IdMembro, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

