

:- initialization(menuRestritoProjeto).
:- use_module("Controllers/Atividades.pl").

clearScreen :- write("\e[H\e[2J"). % só serve no unix

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
        writeln('           M - Voltar ao menu principal                   '),
        writeln('           S - Sair do sistema                            '),  
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, LowerOption),
        processaEntradaMenuRestrito(LowerOption),
        halt. 

processaEntradaMenuRestrito(Entrada) :- 

        clearScreen,

        ( Entrada == 'l' -> visualizarProjetos
        ; Entrada == 'p' -> deletarProjeto
        ; Entrada == 'g' -> gerenciarMembros
        ; Entrada == 'b' -> menuBancoDeAtividades
        ; Entrada == 'i' -> comecarAtividade
        ; Entrada == 'i' -> comecarAtividade
        ; Entrada == 'v' -> visualizarAtividades
        ; Entrada == 'a' -> visualizarStatusAtividade
        ; Entrada == 'o' -> criaFeedback
        ; Entrada == 'm' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).

visualizarProjetos :-
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        retornoMenuRestrito. 
%         visualizarProjetos.

deletarProjeto :-
        writeln('                                                          '),
        writeln('                 |  Remover Projeto:  |                   '),
        writeln('                                                          '), 
        retornoMenuRestrito.

gerenciarMembros :-
        writeln('                                                          '),
        writeln('                                                          '),
        writeln('             |  Gerenciamento de membros:  |              '),
        writeln('                                                          '),
        writeln('                O que deseja fazer agora?                 '),
        writeln('                                                          '),
        writeln('                  Selecione uma opção:                    '),
        writeln('                                                          '),
        writeln('            M - Visualizar membros do projeto             '),
        writeln('            A - Atribuir atividade a um membro            '),
        writeln('            N - Adicionar membro ao projeto               '),
        writeln('            R - Remover membro do projeto                 '),
        writeln('            V - Voltar ao menu principal                  '),
        writeln('            P - Voltar ao menu de projetos                '),
        writeln('            S - Sair do sistema                           '),
        writeln('                                                          '), 

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, LowerOption),
        processaEntradaMembros(LowerOption),
        halt. 

processaEntradaMembros(Entrada) :- 

        clearScreen,

        ( Entrada == 'm' -> vizualizarMembros
        ; Entrada == 'a' -> atribuirAtividade
        ; Entrada == 'n' -> adicionarMembro
        ; Entrada == 'r' -> removerMembro
        ; Entrada == 'p' -> menuRestritoProjeto
        ; Entrada == 'v' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).
        

menuBancoDeAtividades :-
        writeln('                                                          '),
        writeln('           |     Menu Banco de Atividades    |            '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('             C - Criar uma atividade                      '),
        writeln('             R - Remover uma atividade                    '),
        writeln('             L - Listar atividades cadastradas            '),
        writeln('             A - Consultar uma atividade por ID           '),
        writeln('             V - Voltar ao menu principal                 '),
        writeln('             P - Voltar ao menu de projetos               '),
        writeln('             S - Sair do sistema                          '),
        writeln('                                                          '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        
        ( LowerOption == 'c' -> criaAtividade;
        % ; LowerOption == 'r' -> deletaAtividade
        % ; LowerOption == 'l' -> bancoDeAtividades
        % ; LowerOption == 'a' -> consultarAtividade
         LowerOption == 'v' -> consult('Menus/MenuGeral.pl')
        ; LowerOption == 'p' -> menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuGerente ).
        
criaAtividade :- 
        write('O título: '),
        read(Titulo),
        write('A descrição: '),
        read(Descricao),
        write('A dificuldade: '),
        read(Dificuldade),
        write('O ID: '),
        read(IdAtividade),
        salvarAtividade('Database/bancoDeAtividades.json', Titulo, Descricao, Dificuldade, IdAtividade), writeln('Atividade criada!'), 
        retornoMenuRestrito. % está ido para cá: erroMenuGerente, por quê?

sairDoSistema :-
        clearScreen,
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '),
        halt. 

erroMenuGerente :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuRestrito.

% | Retorna ao menu principal ou sai do sistema
retornoMenuRestrito :- 
        writeln('                                                          '),
        writeln(' | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu de projetos                     '),
        writeln('                 P - Menu Principal                       '),
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),

        (LowerOption == 's' -> sairDoSistema, !
                
        ; LowerOption == 'm' -> menuRestritoProjeto, !

        ; LowerOption == 'p' -> consult('Menus/MenuGeral.pl'), !
        
        ; erroMenuGerente ).
