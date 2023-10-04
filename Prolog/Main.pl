%  Menu principal do projeto, que chama todos os demais
:- initialization(main).
main :-
    consult('Menus/MenuGeral.pl'), !.