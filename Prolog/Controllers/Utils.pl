<<<<<<< HEAD
:- module(utils, [nao_vazia/1, ler_string/1, clearScreen/0]).
=======
:- module(utils, [nao_vazia/1, ler_string/1]).
>>>>>>> usuario-prolog

nao_vazia(Input) :-
    Input \= "".

ler_string(X) :-
    read_line_to_codes(user_input, R),
    atom_string(R, X).
<<<<<<< HEAD

clearScreen :- write("\e[H\e[2J"). % só serve no unix
=======
>>>>>>> usuario-prolog
