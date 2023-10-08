:- module(usuario, [lerUsuariosJson/2, usuarioToJSON/4, usuariosToJSON/2, salvarUsuario/4, exibirUsuariosAux/1, 
                    exibirUsuarios/1,getUsuarioJSON/3, removerUsuario/2, removerUsuarioJSON/3, usuarioJaExiste/2]).
:- use_module(library(http/json)).

% Lendo arquivo JSON puro
lerUsuariosJson(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).

% Cria um usuário
usuarioToJSON(Nome, Senha, IdUsuario, Usuario) :-
    swritef(Usuario, '{"nome":"%w", "senha":"%w", "idUsuario":"%w", "atividadesAtribuidas":"%w"}',
[Nome, Senha, IdUsuario, []]).

% Convertendo uma lista de objetos em JSON para 
usuariosToJSON([], []).
usuariosToJSON([H|T], [U|Usuario]) :- 
    usuarioToJSON(H.nome, H.senha, H.idUsuario, U),
    usuariosToJSON(T, Usuario).

% Salvar em arquivo JSON
salvarUsuario(FilePath, Nome, Senha, IdUsuario) :- 
    lerUsuariosJson(FilePath, File),
    usuariosToJSON(File, ListaUsuarios),
    usuarioToJSON(Nome, Senha, IdUsuario, Usuarios),
    append(ListaUsuarios, [Usuarios], Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe os usuarios cadastrados omitindo a senha 
exibirUsuariosAux([]).
exibirUsuariosAux([H|T]) :- 
    write('Nome:'), writeln(H.nome),
    write('ID Usuário:'), writeln(H.idUsuario), 
		nl, exibirUsuariosAux(T).

exibirUsuarios(FilePath) :-
		lerUsuariosJson(FilePath, Usuarios),
		exibirUsuariosAux(Usuarios).

% Pega uma usuario por ID
getUsuarioJSON(IdUsuario, [Usuario|_], Usuario):- IdUsuario =:= Usuario.idUsuario.
getUsuarioJSON(IdUsuario, [_|T], Usuario):- getUsuarioJSON(IdUsuario, T, Usuario).    

% Removendo um usuário - ainda nao funciona
removerUsuarioJSON([], _, []).
removerUsuarioJSON([H|T], H.idUsuario, T).
removerUsuarioJSON([H|T], Id, [H|Out]):- removerUsuarioJSON(T, Id, Out).

removerUsuario(FilePath, Id):-
    lerUsuariosJson(FilePath, File),
    removerUsuarioJSON(File, Id, SaidaParcial),
    usuariosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream),
    writeln('Usuário removido com sucesso. Arquivo atualizado.').


% Predicado para verificar se um usuario com o mesmo IdUsuario já existe no sistema
usuarioJaExiste(IdUsuario, UsuariosDoSistema) :-
    member(usuario(IdUsuario, _, _, _), UsuariosDoSistema).
  

% falta adicionar atividades a um usuario