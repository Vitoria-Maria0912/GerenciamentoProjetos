:- initialization(menuPrincipal).

% | Menu principal com as principais funcionalidades
menuPrincipal :-

        writeln('                                                          '),
        writeln('             |     Menu Principal    |                    '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('            C - Cadastrar novo usuário                    '),
        writeln('            D - Deletar perfil                            '),
        writeln('            P - Criar projeto                             '),
        writeln('            G - Menu de projetos                          '),
        writeln('            M - Caixa de mensagens                        '),
        writeln('            S - Sair do sistema                           '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), % Converter o código ASCII em um caractere
        downcase_atom(Input, LowerOption),
        processaEntradaMenuGeral(LowerOption),
        halt. % encerra o programa

% processaEntradaMenuGeral('c') :-
%          writeln('                                                          '),
%          writeln('                  |     Cadastro:    |                    '),
%          writeln('                                                          '),
%          cadastrarUsuario
%          retornoMenuPrincipal

% processaEntradaMenuGeral('d') :-
%         writeln('                                                          '),
%         writeln('               |     Deletar perfil:    |                 '),
%         writeln('                                                          '),
%         deletarUsuario

% processaEntradaMenuGeral('d') :-
%         writeln('                                                          '),
%         writeln('               |     Criar projeto:    |                  '),
%         writeln('                                                          '),
%         cadastrarProjeto.
%         retornoMenuPrincipal.

% menuProjetos :-
%         writeln('                                                          '),
%         writeln('               |     Menu de projetos:    |               '),
%         writeln('                                                          '),
%         halt. % encerra o programa

% ----------------- Chat ------------------------------
menuChat :- 
        writeln('                                                                        '),
        writeln('                    |      Bem-vindo ao Chat!    |                      '),
        writeln('                                                                        '),
        writeln('  Envie mensagens entre membros do seu projeto e usuários do sistema!   '),
        writeln('                                                                        '),
        writeln('                          Selecione uma opção:                          '),
        writeln('                                                                        '),
        writeln('            C - Visualizar mensagens gerais de um projeto               '),
        writeln('            H - Visualizar mensagens privadas                           '),
        writeln('            A - Enviar mensagem geral para membros do projeto           '),
        writeln('            T - Enviar mensagem privada                                 '),
        writeln('            S - Sair do sistema                                         '),
        writeln('                                                                        '),
        halt.

% processaEntradaMenuGeral('c') :-
%         writeln('                                                          '),
%         writeln('      |     Enviar mensagem para um usuário:    |         '),
%         writeln('                                                          '),
%         enviarMPrivada.
%         retornoMenuPrincipal.

% processaEntradaMenuGeral('c') :-
%         writeln('                                                                 '),
%         writeln('   |     Enviar mensagem para todos os membros do projeto:    |  '),
%         writeln('                                                                 '),
%         enviarMGeral.
%         retornoMenuPrincipal.

% processaEntradaMenuGeral('c') :-
%         writeln('                                                            '),
%         writeln('        |     Mensagens privadas de um usuário:    |        '),
%         writeln('                                                            '),
%         visualizarMensagensPrivadas.
%         retornoMenuPrincipal.


% processaEntradaMenuGeral('c') :-
%         writeln('                                                            '),
%         writeln('         |     Mensagens gerais de um projeto:    |         '),
%         writeln('                                                            '),
%         visualizarMensagensGerais.
%         retornoMenuPrincipal.
        

processaEntradaMenuGeral('s') :-
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '),
        halt. % encerra o programa

processaEntradaMenuGeral(_) :-
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuPublicoProjeto.

% | Retorna ao menu principal ou sai do sistema
retornoMenuPrincipal :- 
        writeln('                                                          '),
        writeln('  | Deseja voltar ao menu principal ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu principal                       '),
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
                menuPrincipal
        ;
                writeln('                                                          '),
                writeln('         |  Entrada Inválida. Tente novamente!  |         '),
                writeln('                                                          '),
                retornoMenuPrincipal
        ).

/**
 * waitInput is det.
 * 
 * Espera o usuário digitar algum caractere.
 */ 
waitInput:-waitInput("").

/**
 * waitInput(+S:string) is det. 
 * 
 * Imprime a mensagem fornecida, esperando o usuário digitar algum caractere.
 * @param S Mensagem a ser impressa antes de esperar a entrada
 */
waitInput(S):-
    ansi_format([bold,fg(yellow)], "~w", [S]),
    ansi_format([bold,fg(yellow)], "~w", ["Aperte qualquer tecla para continuar."]),
    get_single_char(_),nl.
