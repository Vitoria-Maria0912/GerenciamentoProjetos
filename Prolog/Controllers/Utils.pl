:- module(utils, [nao_vazia/1, ler_string/1, clearScreen/0, lerJSON/2]).
:- use_module(library(http/json)).

nao_vazia(Input) :-
    Input \= "".

ler_string(X) :-
    read_line_to_codes(user_input, R),
    atom_string(R, X).

clearScreen :- write("\e[H\e[2J"). % só serve no unix

lerJSON(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).