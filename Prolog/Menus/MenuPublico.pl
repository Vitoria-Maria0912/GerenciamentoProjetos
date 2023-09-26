:- initialization(menuPublicoProjeto).

% | Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :-

        writeln(''),
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
        writeln('            S - Sair do sistema                           '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), % Converter o código ASCII em um caractere
        downcase_atom(Input, LowerOption),
        processaEntrada(LowerOption),
        % nl. % dá uma quebra de linha
        halt. % encerra o programa

processaEntrada('l') :-
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        %       visualizarProjetos.
        retornoMenuPublico.

% processaEntrada('i') :-
%         comecarAtividade 

% processaEntrada('f') :-
%         finalizarAtividade 

% processaEntrada('v') :-
%          visualizarAtividades

% processaEntrada('a') :-
%          statusAtividade

% processaEntrada('o') :-
%          criaFeedback

processaEntrada('s') :-
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '),
        halt. % encerra o programa

processaEntrada(_) :-
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuPublicoProjeto.


% | Retorna ao menu principal ou sai do sistema
retornoMenuPublico :- 
        writeln('                                                          '),
        writeln(' | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu de projetos                     '),
        writeln('                 S - Sair do sistema                      '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), % Converter o código ASCII em um caractere
        downcase_atom(Input, LowerOption),
        (LowerOption == 's' ->
                writeln('                                                          '),
                writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
                writeln('                                                          '),
                halt % encerra o programa
        ; LowerOption == 'm' ->
                menuPublicoProjeto
        ;
                writeln('                                                          '),
                writeln('         |  Entrada Inválida. Tente novamente!  |         '),
                writeln('                                                          '),
                retornoMenuPublico
        ).