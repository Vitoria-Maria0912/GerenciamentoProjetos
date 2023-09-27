:- initialization(menuPrincipal).

% | Menu principal com as principais funcionalidades
menuPrincipal :-

        writeln('                                                          '),
        writeln('               |     Menu Principal    |                  '),
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
        processaEntrada(LowerOption),
        halt. % encerra o programa

% processaEntrada('c') :-
%          writeln('                                                          '),
%          writeln('                  |     Cadastro:    |                    '),
%          writeln('                                                          '),
%          cadastrarUsuario
%          retornoMenuPrincipal

% processaEntrada('d') :-
%         writeln('                                                          '),
%         writeln('               |     Deletar perfil:    |                 '),
%         writeln('                                                          '),
%         deletarUsuario

% processaEntrada('d') :-
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

% processaEntrada('c') :-
%         writeln('                                                          '),
%         writeln('      |     Enviar mensagem para um usuário:    |         '),
%         writeln('                                                          '),
%         enviarMPrivada.
%         retornoMenuPrincipal.

% processaEntrada('c') :-
%         writeln('                                                                 '),
%         writeln('   |     Enviar mensagem para todos os membros do projeto:    |  '),
%         writeln('                                                                 '),
%         enviarMGeral.
%         retornoMenuPrincipal.

% processaEntrada('c') :-
%         writeln('                                                            '),
%         writeln('        |     Mensagens privadas de um usuário:    |        '),
%         writeln('                                                            '),
%         visualizarMensagensPrivadas.
%         retornoMenuPrincipal.


% processaEntrada('c') :-
%         writeln('                                                            '),
%         writeln('         |     Mensagens gerais de um projeto:    |         '),
%         writeln('                                                            '),
%         visualizarMensagensGerais.
%         retornoMenuPrincipal.
        

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