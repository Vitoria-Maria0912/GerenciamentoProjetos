:- module(utils, [nao_vazia/1, ler_string/1]).

nao_vazia(Input) :-
    Input \= "".

ler_string(X) :-
    read_line_to_codes(user_input, R),
    atom_string(R, X).