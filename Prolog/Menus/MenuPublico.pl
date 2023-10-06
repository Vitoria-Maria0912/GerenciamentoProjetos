:- initialization(menuPublicoProjeto).

clearScreen :- write("\e[H\e[2J").

% | Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :-

        writeln('                                                          '),
        writeln('               |     Menu Projeto    |                    '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('            L - Listar projetos cadastrados               '),
        writeln('            I - Iniciar uma atividade                     '),
        writeln('            F - Finalizar uma atividade                   '),
        writeln('            V - Visualizar atividades do projeto          '),
        writeln('            A - Visualizar status de uma atividade        '),
        writeln('            O - Dar feedback em uma atividade             '),
        writeln('            M - Voltar ao menu principal                  '),
        writeln('            S - Sair do sistema                           '),
        writeln('                                                          '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntrada(LowerOption),
        halt. 

processaEntrada(Entrada) :- 

        clearScreen,

        ( Entrada == 'l' -> visualizarProjetos
        ; Entrada == 'i' -> comecarAtividade
        ; Entrada == 'f' -> finalizarAtividade
        ; Entrada == 'v' -> visualizarAtividades
        ; Entrada == 'a' -> statusAtividade
        ; Entrada == 'o' -> criaFeedback
        ; Entrada == 'm' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPublico ).


visualizarProjetos :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        % visualizarProjetos.
        retornoMenuPublico.


sairDoSistema :-
        clearScreen,
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '), !. 

erroMenuPublico :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuPublicoProjeto.

% | Retorna ao menu principal ou sai do sistema
retornoMenuPublico :- 
        writeln('                                                          '),
        writeln(' | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu Principal                       '),
        writeln('                 P - Menu de Projetos                     '),
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),

        (LowerOption == 's' ->
                sairDoSistema

        ; LowerOption == 'p' -> menuPublicoProjeto, !

        ; LowerOption == 'm' -> clearScreen, consult('Menus/MenuGeral.pl'), !
        
        ;       clearScreen,
                writeln('                                                          '),
                writeln('         |  Entrada Inválida. Tente novamente!  |         '),
                writeln('                                                          '),
                retornoMenuPublico, !
        ).

% Menu do banco
menuBancoDeAtividades :-
        writeln('                                                         '),
        writeln('           |  Menu Banco de Atividades  |                '),
        writeln('                                                         '),
        writeln('                 Selecione uma opção:                    '),
        writeln('                                                         '),
        writeln('            C - Criar uma atividade                      '),
        writeln('            L - Listar atividades cadastradas            '),
        writeln('            M - Voltar ao menu de projetos               '),
        writeln('            V - Voltar ao menu principal                 '),
        writeln('            S - Sair do sistema                          '),
        writeln('                                                         '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, LowerOption),
        processaEntradaBancoDeAtividades(LowerOption),
        retornoMenuRestrito. 
        

processaEntradaBancoDeAtividades(Entrada) :- 

        clearScreen,

        ( Entrada == 'c' -> criaAtividade
        ; Entrada == 'r' -> deletaAtividade
        ; Entrada == 'l' -> bancoDeAtividades
        ; Entrada == 'a' -> consultarAtividade
        ; Entrada == 'm' -> menuPublicoProjeto
        ; Entrada == 'v' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPublico ).