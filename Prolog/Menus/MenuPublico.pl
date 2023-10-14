:- module(menuPublico, [menuPublicoProjeto/0, processaEntradaMenuPublico/1, visualizarProjetos/0, sairDoSistema/0 , 
                        erroMenuPublico/0 , retornoMenuPublico/0 , menuPublicoBancoDeAtividades/0 , processaEntradaBancoDeAtividades/1,
                        criaAtividade/0, listarAtividades/0, comecarAtividade/0, finalizarAtividade/0, visualizarStatusAtividade/0,
                        consultarAtividade/0, criaFeedback/0]).

:- use_module("Controllers/Atividades.pl").
:- use_module("Controllers/Utils.pl").

% | Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :-

        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('            L - Listar projetos cadastrados               '), 
        writeln('            B - Menu do banco de atividades               '),  
        writeln('            M - Voltar ao menu principal                  '),
        writeln('            S - Sair do sistema                           '),
        writeln('                                                          '),       
   
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntradaMenuPublico(LowerOption),
        halt. 

processaEntradaMenuPublico(Entrada) :- 

        clearScreen,

        ( Entrada == 'l' -> visualizarProjetos
        ; Entrada == 'b' -> menuPublicoBancoDeAtividades
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
        writeln('                                                          '), 
        halt.

erroMenuPublico :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuPublico.

% | Retorna ao menu principal ou sai do sistema
retornoMenuPublico :- 
        writeln('                                                          '),
        writeln('       | Deseja voltar ao menu ou sair do sistema?  |     '),
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
menuPublicoBancoDeAtividades :-
        writeln('                                                         '),
        writeln('           |  Menu Banco de Atividades  |                '),
        writeln('                                                         '),
        writeln('                 Selecione uma opção:                    '),
        writeln('                                                         '),
        writeln('            L - Listar atividades cadastradas            '),
        writeln('            C - Criar uma atividade                      '),
        writeln('            I - Iniciar uma atividade                    '), 
        writeln('            F - Finalizar uma atividade                  '),
        writeln('            V - Visualizar atividades do projeto         '),
        writeln('            A - Visualizar status de uma atividade       '),
        writeln('            D - Consultar uma atividade por ID           '),
        writeln('            O - Dar feedback em uma atividade            '),
        writeln('            P - Voltar ao menu de projetos               '),
        writeln('            M - Voltar ao menu principal                 '),
        writeln('            S - Sair do sistema                          '),
        writeln('                                                         '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, LowerOption),
        processaEntradaBancoDeAtividades(LowerOption),
        retornoMenuPublico. 
        

processaEntradaBancoDeAtividades(Entrada) :- 

        clearScreen,

        ( Entrada == 'l' -> clearScreen, listarAtividades, retornoMenuPublico
        ; Entrada == 'c' -> clearScreen, criaAtividade, retornoMenuPublico
        ; Entrada == 'i' -> clearScreen, comecarAtividade, retornoMenuPublico
        ; Entrada == 'f' -> clearScreen, finalizarAtividade, retornoMenuPublico
        ; Entrada == 'v' -> clearScreen, visualizarAtividades, retornoMenuPublico
        ; Entrada == 'a' -> clearScreen, visualizarStatusAtividade, retornoMenuPublico
        ; Entrada == 'd' -> clearScreen, consultarAtividade, retornoMenuPublico
        ; Entrada == 'o' -> clearScreen, criaFeedback, retornoMenuPublico
        % ; Entrada == 'p' -> % tem que modificar para que volte para o MenuGerente também/
        ; Entrada == 'm' -> menuPrincipal 
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPublico ).

criaAtividade :- 
        writeln('                                                       '),
        writeln('               |  Criar atividade:  |                  '),
        writeln('                                                       '),
        
        write('Digite um título para sua atividade: '),
        ler_string(Titulo), nl,
        write('Descreva, brevemente, o que se deve realizar para concluir esta atividade. '),
        ler_string(Descricao), nl,
        write('Digite qual a complexidade para realizá-la (Fácil/Média/Difícil): '),
        ler_string(Dificuldade), nl,

        (nao_vazia(Titulo), nao_vazia(Descricao), nao_vazia(Dificuldade) -> 

                random(10000, 99999, Idatom),
                atom_string(Idatom,IdAtividade),
                lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),

                (\+ atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                        salvarAtividade('Database/bancoDeAtividades.json', Titulo, Descricao, Dificuldade, IdAtividade), 
                        write('Atividade criada! E o ID dela é: '), writeln(IdAtividade), nl
                        
                ; erroMenuGerente)

        ; clearScreen,
          writeln('                                                                                                             '),
          writeln(' |  Você deixou um campo obrigatório vazio, não foi possível criar a atividade, tente novamente!  |          '),
          criaAtividade
        ).


listarAtividades :-
        writeln('                                                       '),
        writeln('          |  Listar Atividades cadastradas:  |         '),
        writeln('                                                       '),
        exibirAtividades('Database/bancoDeAtividades.json'). 

comecarAtividade :-
        writeln('                                                          '),
        writeln('                |  Começar atividade:  |                  '),
        writeln('                                                          '),

        writeln('Digite seu ID:'),
        ler_string(IdUsuario),

        lerUsuariosJson('Database/usuarios.json', UsuariosDoSistema),

        (verifica_id(IdUsuario, UsuariosDoSistema) ->

                writeln('Digite o ID da atividade que deseja começar: '),
                ler_string(IdAtividade),

                lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),


                (atividadeJaExiste(IdAtividade, AtividadesDoSistema) ->

                        getAtividadeJSON(IdAtividade, Atividade),

                        (Atividade.idMembroResponsavel \= IdUsuario ->
                                editarStatusAtividade('Database/bancoDeAtividade.json', IdAtividade, 'Pendente...')
                        
                        ; clearScreen,
                          writeln('                                                          '),
                          writeln('      |  Você não está atribuído a essa atividade!  |     '),
                          writeln('                                                          ')
                        )

                ; clearScreen,
                  writeln('                                                          '),
                  writeln('           |  Atividade inexistente! Tente novamente.  |  '),
                  writeln('                                                          ')
                )

                % writeln('                                                          '),
                % writeln('          |  Esta atividade já foi concluída!  |          '),
                % writeln('                                                          '),

                

                % writeln('                                                          '),
                % writeln('         |  Esta atividade já está em andamento!  |       '),
                % writeln('                                                          ')
        
                
        ; clearScreen,
          writeln('                                                          '),
          writeln('           |  ID incorreto! Tente novamente.  |           '),
          writeln('                                                          ')
        ).

finalizarAtividade:-

        writeln('                                                          '),
        writeln('              |  Finalizar atividade:  |                  '),
        writeln('                                                          '),

        writeln('Digite o ID da atividade que deseja começar: '),
        ler_string(IdAtividade),

        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),

        (atividadeJaExiste(IdAtividade, AtividadesDoSistema) ->
                        editarStatusAtividade('Database/bancoDeAtividade.json', IdAtividade, 'Concluída!')
        ; erroMenuPublico).

        writeln('                                                          '),
        writeln('          |  Esta atividade já foi concluída!  |                  '),
        writeln('                                                          '),

        writeln('                                                          '),
        writeln('      |  Você não está atribuído a essa atividade!  |                  '),
        writeln('                                                          '),

        writeln('                                                          '),
        writeln('           |  ID incorreto! Tente novamente.  |                  '),
        writeln('                                                          ').

visualizarStatusAtividade:-
        writeln('                                                          '),
        writeln('           |  Mostrar status da atividade:  |             '),
        writeln('                                                          '),
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),  
        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        (atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
                write(Atividade)
        ; erroMenuGerente).

consultarAtividade:-

        writeln('                                                          '),
        writeln('                |  Mostrar Atividade:  |                  '),
        writeln('                                                          '),
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),  

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        (atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
                write(Atividade)
        ; erroMenuGerente).

criaFeedback:-
        writeln('                                                                       '),
        writeln('   |  Comente sobre uma atividade que você criou ou foi designado:  |  '),
        writeln('                                                                       '),
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),  

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,
        
        (atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                write('Escreva um breve comentário sobre a atividade: '),
                ler_string(Feedback), nl,

                % so quem cria ou é designado pode
                writeln('                                                                 '),
                writeln('      |  Você não está autorizado a realizar esta ação!  |                  '),
                writeln('                                                                 '),
        
                writeln('                                                          '),
                writeln('           |  ID incorreto! Tente novamente.  |                  '),
                writeln('                                                          '),

                writeln('                                                          '),
                writeln('   |  Comentário adicionado com sucesso a atividade de ID '), IdAtividade, write(' |                  '),
                writeln('                                                          ')

                % MOSTRAR A ATIVIDADE COM O FEEDBACK

                % addFeedback ----- PRECISA CRIAR AINDA
        ; erroMenuGerente).
