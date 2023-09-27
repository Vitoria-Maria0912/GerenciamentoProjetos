:- initialization(menuRestritoProjeto).

% | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :-

        writeln(''),
        writeln('               |     Menu Projeto    |                    '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('           L - Listar projetos cadastrados                '), 
        writeln('           P - Remover projeto                            '),  
        writeln('           G - Gerenciar membros do projeto               '),  
        writeln('           B - Menu do banco de atividades                '),  
        writeln('           I - Iniciar uma atividade                      '),  
        writeln('           F - Finalizar uma atividade                    '),  
        writeln('           V - Visualizar atividades do projeto           '),  
        writeln('           A - Visualizar status de uma atividade         '),  
        writeln('           O - Dar feedback em uma atividade              '),  
        writeln('           S - Sair do sistema                            '),  
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), % Converter o código ASCII em um caractere
        downcase_atom(Input, LowerOption),
        processaEntrada(LowerOption),
        halt. % encerra o programa

% processaEntrada('l') :-
%         writeln('                                                          '),
%         writeln('         |  Estes são os projetos no sistema:  |          '),
%         writeln('                                                          '),
%         visualizarProjetos.
%         retornoMenuPublico.

% processaEntrada('p') :-
%         writeln('                                                          '),
%         writeln('                 |  Remover Projeto:  |                   '),
%         writeln('                                                          '),
%         deletarProjeto.

% processaEntrada('g') :-
%         writeln(''),
%         writeln('                                                          '),
%         writeln('             |  Gerenciamento de membros:  |              '),
%         writeln('                                                          '),
%         writeln('                O que deseja fazer agora?                 '),
%         writeln('                                                          '),
%         writeln('                  Selecione uma opção:                    '),
%         writeln('                                                          '),
%         writeln('            M - Visualizar membros do projeto             '),
%         writeln('            A - Atribuir atividade a um membro            '),
%         writeln('            N - Adicionar membro ao projeto               '),
%         writeln('            R - Remover membro do projeto                 '),
%         writeln('            V - Voltar ao menu do projeto                 '),
%         writeln('                                                          '),
%         gerenciarMembros.

% processaEntrada('b') :-
%         writeln(''),
%         writeln('           |     Menu Banco de Atividades    |            '),
%         writeln('                                                          '),
%         writeln('                 Selecione uma opção:                     '),
%         writeln('                                                          '),
%         writeln('             C - Criar uma atividade                      '),
%         writeln('             R - Remover uma atividade                    '),
%         writeln('             L - Listar atividades cadastradas            '),
%         writeln('             A - Consultar uma atividade por ID           '),
%         writeln('             V - Voltar ao menu principal                 '),
%         writeln('             S - Sair do sistema                          '),
%         writeln('                                                          '),
%         menuBancoDeAtividades.

processaEntrada('s') :-
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '),
        halt. % encerra o programa

processaEntrada(_) :-
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuRestritoProjeto.

% | Retorna ao menu principal ou sai do sistema
retornoMenuRestrito :- 
        writeln('                                                          '),
        writeln(' | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu de projetos                     '),
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), % Converter o código ASCII em um caractere
        downcase_atom(Input, LowerOption),
        (LowerOption == 's' ->
                writeln('                                                          '),
                writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
                writeln('                                                          '),
                halt % encerra o programa
        ; LowerOption == 'm' ->
                menuRestritoProjeto
        ;
                writeln('                                                          '),
                writeln('         |  Entrada Inválida. Tente novamente!  |         '),
                writeln('                                                          '),
                retornoMenuRestrito
        ).