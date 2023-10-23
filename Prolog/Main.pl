%  Menu principal do projeto, que chama todos os demais
:- initialization(main).

main :-
    shell('clear'),
    consult('Menus/MenuGeral.pl'), !.