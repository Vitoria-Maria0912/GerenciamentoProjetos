:- module(usuario, [usuarioToJSON/5, usuariosToJSON/2, salvarUsuario/5, exibirUsuariosAux/1, 
                    exibirUsuarios/1,getUsuarioJSON/3, removerUsuario/2, removerUsuarioJSON/3, 
                    verifica_id/3, editarAtividades/3, exibirUsuario/1,exibeUsuarios_id_nome/1,
                    exibirUsuarios_id_nome_aux/1, removerUsuarioDeProjetos/1]).
:- use_module(library(http/json)).
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Projeto.pl").


% Cria um usuário
usuarioToJSON(Nome, Senha, IdUsuario, Atividades, Usuario) :-
    swritef(Usuario, '{"nome":"%w", "senha":"%w", "idUsuario":"%w", "atividadesAtribuidas":%w}',
[Nome, Senha, IdUsuario, Atividades]).

% Convertendo uma lista de objetos em JSON para 
usuariosToJSON([], []).
usuariosToJSON([H|T], [U|Usuario]) :- 
    usuarioToJSON(H.nome, H.senha, H.idUsuario, H.atividadesAtribuidas, U),
    usuariosToJSON(T, Usuario).

% Salvar em arquivo JSON
salvarUsuario(FilePath, Nome, Senha, IdUsuario, Atividades) :- 
    lerJSON(FilePath, File),
    usuariosToJSON(File, ListaUsuarios),
    usuarioToJSON(Nome, Senha, IdUsuario, Atividades, Usuarios),
    append(ListaUsuarios, [Usuarios], Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe os usuarios cadastrados omitindo a senha 
exibirUsuariosAux([]).
exibirUsuariosAux([H|T]) :- 
    write('|- ID Usuário: '), writeln(H.idUsuario), 
    write('|- Nome: '), writeln(H.nome),
    write('|- Atividades atribuídas: '), writeln(H.atividadesAtribuidas),    
    nl, exibirUsuariosAux(T).

exibirUsuarios(FilePath) :-
		lerJSON(FilePath, Usuarios),
		exibirUsuariosAux(Usuarios).

% Pega uma usuario por ID
getUsuarioJSON(IdUsuario, [Usuario|_], Usuario):- IdUsuario == Usuario.idUsuario.
getUsuarioJSON(IdUsuario, [_|T], Usuario):- getUsuarioJSON(IdUsuario, T, Usuario).   

% Exibe um usuario
exibirUsuario(Usuario) :-
    write('|- ID Usuário: '), writeln(Usuario.idUsuario), 
    write('|- Nome: '), writeln(Usuario.nome),nl.

% Removendo um usuário - ainda nao funciona
removerUsuarioJSON([], _, []).
removerUsuarioJSON([H|T], H.idUsuario, T).
removerUsuarioJSON([H|T], Id, [H|Out]):- removerUsuarioJSON(T, Id, Out).

removerUsuario(FilePath, Id):-
    lerJSON(FilePath, File),
    removerUsuarioJSON(File, Id, SaidaParcial),
    usuariosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream),
    writeln('         |    Usuário removido com sucesso. Até a próxima!    |').

% verifica se um id existe dentro da lista de usuarios
verifica_id(_, [], false).
verifica_id(Busca, [Usuario|_], true) :- 
    get_dict(idUsuario, Usuario, Id),
    Busca == Id.
verifica_id(Busca, [_|T], R) :- verifica_id(Busca, T, R).


editarAtividadesJSON([], _, _, []).
editarAtividadesJSON([H|T], H.idUsuario, NovaAtividade, [NovoUsuario|T]) :-
    append(H.atividadesAtribuidas, [NovaAtividade], NovaListaAtividades),
    NovoUsuario = _{
        idUsuario:H.idUsuario,
        nome:H.nome,
        senha:H.senha,
        atividadesAtribuidas:NovaListaAtividades
    }.
editarAtividadesJSON([H|T], Id, NovaAtividade, [H|Out]) :- editarAtividadesJSON(T, Id, NovaAtividade, Out).

editarAtividades(FilePath, IdU, NovaAtividade) :-
    lerJSON(FilePath, File),
    editarAtividadesJSON(File, IdU, NovaAtividade, SaidaParcial),
    usuariosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% | Exibe apenas nome e ID dos usuários
exibeUsuarios_id_nome(FilePath):-
    lerJSON(FilePath, Usuarios),
    exibirUsuarios_id_nome_aux(Usuarios).

exibirUsuarios_id_nome_aux([]).
exibirUsuarios_id_nome_aux([Usuario|T]) :-
    exibirUsuario(Usuario), nl, exibirUsuarios_id_nome_aux(T).

listaProjetos(_, [], []).
listaProjetos(IdUsuario, [Projeto|OutrosProjetos], [IdProjeto|OutrosIdsProjetos]) :-
    ((string_para_numero(IdUsuario, Idfake), member(Idfake, Projeto.membros)) -> 
    IdProjeto = Projeto.idProjeto,
    listaProjetos(IdUsuario, OutrosProjetos, OutrosIdsProjetos);
      listaProjetos(IdUsuario, OutrosProjetos, OutrosIdsProjetos)
    ).

removerUsuarioDeProjetosAux([], _).
removerUsuarioDeProjetosAux([IdProjeto | Resto], IdUsuario) :-
    removerMembro('Database/projetos.json', IdProjeto, IdUsuario),
    removerUsuarioDeProjetosAux(Resto, IdUsuario).

removerUsuarioDeProjetos(IdUsuario) :-
    lerJSON('Database/projetos.json', Projetos),
    listaProjetos(IdUsuario, Projetos, ListaProjetos),
    removerUsuarioDeProjetosAux(ListaProjetos, IdUsuario).

