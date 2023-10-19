:- module(usuario, [usuarioToJSON/5, usuariosToJSON/2, salvarUsuario/5, exibirUsuariosAux/1, 
                    exibirUsuarios/1,getUsuarioJSON/3, removerUsuario/2, removerUsuarioJSON/3, verifica_id/3, editarAtividades/3]).
:- use_module(library(http/json)).
:- use_module("Controllers/Utils.pl").


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
    write('Nome: '), writeln(H.nome),
    write('ID Usuário: '), writeln(H.idUsuario), 
		nl, exibirUsuariosAux(T).

exibirUsuarios(FilePath) :-
		lerJSON(FilePath, Usuarios),
		exibirUsuariosAux(Usuarios).

% Pega uma usuario por ID
getUsuarioJSON(IdUsuario, [Usuario|_], Usuario):- IdUsuario == Usuario.idUsuario.
getUsuarioJSON(IdUsuario, [_|T], Usuario):- getUsuarioJSON(IdUsuario, T, Usuario).    

% Removendo um usuário - ainda nao funciona
removerUsuarioJSON([], _, []).
removerUsuarioJSON([H|T], H.idUsuario, T).
removerUsuarioJSON([H|T], Id, [H|Out]):- removerUsuarioJSON(T, Id, Out).

removerUsuario(FilePath, Id):-

    lerJSON(FilePath, File),
    removerUsuarioJSON(File, Id, SaidaParcial),
    usuariosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream),
    writeln('Usuário removido com sucesso. Até a próxima!').

% verifica se um id existe dentro da lista de usuarios
verifica_id(_, [], false).
verifica_id(Busca, [Usuario|_], true) :- 
    get_dict(idUsuario, Usuario, Id),
    Busca == Id.
verifica_id(Busca, [_|T], R) :- verifica_id(Busca, T, R).


% falta testar - Adicionando atividades a lista de atividades atribuidas de um usuário
editarAtivUsuarioJSON([], _, _, []).
editarAtivUsuarioJSON([H|T], H.id, IdAtividade, [NovoUsuario|T]):-
    NovoUsuario = _{id:H.id, atividadesAtribuidas:[IdAtividade|H.atividadesAtribuidas], nome:H.nome, senha:H.senha}.
editarAtivUsuarioJSON([H|T], Id, IdAtividade, [H|Out]) :- 
		editarAtivUsuarioJSON(T, Id, IdAtividade, Out).

addAtividadeUsuario(FilePath, IdUsuario, IdAtividade) :-
		lerUsuariosJSON(FilePath, File),
		editarAtivUsuarioJSON(File, IdUsuario, IdAtividade, SaidaParcial),
		usuariosToJSON(SaidaParcial, Saida),
		open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

editarAtividadesJSON([], _, _, []).
editarAtividadesJSON([H|T], H.idUsuario, NovaAtividade, [_{idUsuario:H.idUsuario, nome:H.nome, senha:H.senha, atividadesAtribuidas:NovaListaAtividades}|T]) :-
    adicionarAtividade(H.atividadesAtribuidas, NovaAtividade, NovaListaAtividades).
editarAtividadesJSON([H|T], Id, NovaAtividade, [H|Out]) :- editarAtividadesJSON(T, Id, NovaAtividade, Out).

adicionarAtividade(ListaAtividades, NovaAtividade, NovaListaAtividades) :-
    NovaListaAtividades = [NovaAtividade|ListaAtividades].

editarAtividades(FilePath, IdU, NovaAtividade) :-
    lerJSON(FilePath, File),
    editarAtividadesJSON(File, IdU, NovaAtividade, SaidaParcial),
    usuariosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).
