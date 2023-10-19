:- module(utils, [nao_vazia/1, ler_string/1, clearScreen/0, lerJSON/2, verificaSenhaIdUsuario/3, sairDoSistema/0]).

:- use_module(library(http/json)).
:- use_module("Controllers/Usuario.pl").


nao_vazia(Input) :-
    Input \= "".

ler_string(X) :-
    read_line_to_codes(user_input, R),
    atom_string(R, X).

clearScreen :- write("\e[H\e[2J"). % só serve no unix

lerJSON(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).

verificaSenhaIdUsuario(IdUsuario, Senha, Usuarios) :-
    getUsuarioJSON(IdUsuario, Usuarios, Usuario),
    Usuario.senha == Senha.


sairDoSistema :-
    clearScreen,
    writeln('                                                          '),
    writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
    writeln('                                                          '), 
    halt.

