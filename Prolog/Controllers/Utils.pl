:- module(utils, [nao_vazia/1, ler_string/1, clearScreen/0, lerJSON/2, obterHoraAtual/1, verificaSenhaIdUsuario/3, gerenteDoProjeto/3, string_presente/2, sairDoSistema/0, string_para_numero/2]).

:- use_module(library(http/json)).
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module(library(system)).

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

gerenteDoProjeto(IdProjeto, IdUsuario, Projetos) :-
    getProjetoJSON(IdProjeto, Projetos, Projeto),
    Projeto.idGerente == IdUsuario.

string_para_numero(String, Numero) :-
    number_string(Numero, String).


sairDoSistema :-
    clearScreen,
    writeln('                                                          '),
    writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
    writeln('                                                          '), 
    halt.
