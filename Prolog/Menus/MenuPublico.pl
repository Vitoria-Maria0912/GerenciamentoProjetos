:- module(menuPublico, [menuPublicoProjeto/0, processaEntradaMenuPublico/1, visualizarProjetos/0, erroMenuProjeto/0, 
                        retornoMenuProjetos/0 , menuPublicoBancoDeAtividades/0 , processaEntradaBancoDeAtividades/1,
                        criaAtividade/0, listarAtividades/0, comecarAtividade/0, finalizarAtividade/0, visualizarStatusAtividade/0,
                        consultarAtividade/0, criaFeedback/0, visualizarAtividadesDoProjeto/0]).

<<<<<<< HEAD
=======
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Atividades.pl").
>>>>>>> main
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Atividades.pl").

<<<<<<< HEAD
% | Exibe uma mensagem de erro e volta ao menu de projetos 
erroMenuProjeto :-
=======
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

        ( Entrada == 'l' -> clearScreen, visualizarProjetos
        ; Entrada == 'b' -> clearScreen, menuPublicoBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPublico ).


visualizarProjetos :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        exibirProjetos('Database/projetos.json').
        retornoMenuPublico.


erroMenuPublico :-
>>>>>>> main
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuProjetos.

% | Retorna ao menu principal ou sai do sistema
retornoMenuProjetos :- 
        writeln('                                                          '),
        writeln('       | Deseja voltar ao menu ou sair do sistema?  |     '), nl,
        writeln('                                                          '),
        writeln('                 M - Menu Principal                       '), nl,
        writeln('                 P - Menu de Projetos                     '), nl,
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),

        (LowerOption == 's' -> sairDoSistema

<<<<<<< HEAD
        ; LowerOption == 'p' -> clearScreen, menuProjetos

        ; LowerOption == 'm' -> clearScreen, menuPrincipal
        
        ; erroMenuProjeto).
=======
        ; LowerOption == 'p' -> clearScreen, menuPublicoProjeto

        ; LowerOption == 'm' -> clearScreen, consult('Menus/MenuGeral.pl')
        
        ;       clearScreen,
                writeln('                                                          '),
                writeln('         |  Entrada Inválida. Tente novamente!  |         '),
                writeln('                                                          '),
                retornoMenuPublico
        ).
>>>>>>> main

% | Menu dos projetos, todos os usuários tem acesso
menuPublicoProjeto :-

        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '), nl,
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '), nl,
        writeln('                                                          '), 
        writeln('            L - Listar projetos cadastrados               '), nl, 
        writeln('            B - Menu do banco de atividades               '), nl,  
        writeln('            M - Voltar ao menu principal                  '), nl,
        writeln('            S - Sair do sistema                           '), 
        writeln('                                                          '),     
   
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, Entrada),

        ( Entrada == 'l' -> clearScreen, visualizarProjetos
        ; Entrada == 'b' -> clearScreen, menuPublicoBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto),

        retornoMenuProjetos.

% | Exibe todos os projetos do sistema
visualizarProjetos :-
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '), nl,
        exibirProjetos('Database/projetos.json').

% | Menu do banco, com opções limitadas
menuPublicoBancoDeAtividades :-
        writeln('                                                         '),
        writeln('           |  Menu Banco de Atividades  |                '), nl,
        writeln('                                                         '),
        writeln('                 Selecione uma opção:                    '), nl,
        writeln('                                                         '),
        writeln('            L - Listar atividades cadastradas            '), nl,
        writeln('            C - Criar uma atividade                      '), nl,
        writeln('            I - Iniciar uma atividade                    '), nl, 
        writeln('            F - Finalizar uma atividade                  '), nl,
        writeln('            V - Visualizar atividades do projeto         '), nl,
        writeln('            A - Visualizar status de uma atividade       '), nl,
        writeln('            D - Consultar uma atividade por ID           '), nl,
        writeln('            O - Dar feedback em uma atividade            '), nl,
        writeln('            M - Voltar ao menu principal                 '), nl,
        writeln('            S - Sair do sistema                          '), nl,
        writeln('                                                         '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, Entrada),

<<<<<<< HEAD
        ( Entrada == 'l' -> clearScreen, listarAtividades
        ; Entrada == 'c' -> clearScreen, criaAtividade
        ; Entrada == 'i' -> clearScreen, comecarAtividade
        ; Entrada == 'f' -> clearScreen, finalizarAtividade
        ; Entrada == 'v' -> clearScreen, visualizarAtividadesDoProjeto
        ; Entrada == 'a' -> clearScreen, visualizarStatusAtividade
        ; Entrada == 'd' -> clearScreen, consultarAtividade
        ; Entrada == 'o' -> clearScreen, criaFeedback
=======
processaEntradaBancoDeAtividades(Entrada) :- 

        ( Entrada == 'l' -> clearScreen, listarAtividades, retornoMenuPublico
        ; Entrada == 'c' -> clearScreen, criaAtividade, retornoMenuPublico
        ; Entrada == 'i' -> clearScreen, comecarAtividade, retornoMenuPublico
        ; Entrada == 'f' -> clearScreen, finalizarAtividade, retornoMenuPublico
        ; Entrada == 'v' -> clearScreen, visualizarAtividades, retornoMenuPublico
        ; Entrada == 'a' -> clearScreen, visualizarStatusAtividade, retornoMenuPublico
        ; Entrada == 'd' -> clearScreen, consultarAtividade, retornoMenuPublico
        ; Entrada == 'o' -> clearScreen, criaFeedback, retornoMenuPublico
>>>>>>> main
        ; Entrada == 'm' -> clearScreen, menuPrincipal 
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto).

% | Exibe todas as atividades do sistema
listarAtividades :-
        writeln('                                                '),
        writeln('          |  Atividades cadastradas:  |         '),
        writeln('                                                '), nl,
        exibirAtividades('Database/bancoDeAtividades.json').

% | Cria uma atividade apenas no banco de atividades, sem atribuir a nenhum projeto 
criaAtividade :- 

        listarAtividades,

        writeln('                                                       '),
        writeln('               |  Criar atividade:  |                  '),
        writeln('                                                       '), nl,
        
        write('Digite um título para sua atividade: '),
        ler_string(Titulo), nl,
        write('Descreva, brevemente, o que se deve realizar para concluir esta atividade. '),
        ler_string(Descricao), nl,
        write('Digite qual a complexidade para realizá-la (Fácil/Média/Difícil): '),
        ler_string(Dificuldade), nl,

        (nao_vazia(Titulo), nao_vazia(Descricao), nao_vazia(Dificuldade) -> 

                random(10000, 99999, Idatom),
                atom_string(Idatom,IdAtividade),
                lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
                verifica_id_atividade(IdAtividade, AtividadesDoSistema, Existe),
                (\+ Existe -> 
                        salvarAtividade('Database/bancoDeAtividades.json', Titulo, Descricao, Dificuldade, IdAtividade, 'Não atribuída!', 'Não atribuído!', 'Não atribuído!', []), 
                        write('Atividade criada! E o ID dela é: '), writeln(IdAtividade), nl
                        
<<<<<<< HEAD
=======
                ; erroMenuGerente)

        ; clearScreen,
          writeln('                                                                                                             '),
          writeln(' |  Você deixou um campo obrigatório vazio, não foi possível criar a atividade, tente novamente!  |          '),
          criaAtividade
        ).

listarAtividades :-
        writeln('                                                '),
        writeln('          |  Atividades cadastradas:  |         '),
        writeln('                                                '),
        exibirAtividades('Database/bancoDeAtividades.json'). 

comecarAtividade :-
        writeln('                                                      '),
        writeln('            |  Começar atividade:  |                  '),
        writeln('                                                      '),

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        (ExisteUsuario ->

                listarAtividades, nl,

                write('Digite o ID da atividade que deseja começar: '),
                ler_string(IdAtividade), nl,

                lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
                atividadeJaExiste(IdAtividade, AtividadesDoSistema, ExisteAtividade),

                (ExisteAtividade ->

                        getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),

                        (Atividade.idMembroResponsavel == IdUsuario ->
                                editarStatusAtividade('Database/bancoDeAtividade.json', IdAtividade, 'Pendente...'),
                                exibirAtividade(Atividade)
                        
                        ; clearScreen,
                          writeln('                                                           '),
                          writeln('       |  Você não está atribuído a essa atividade!  |     '),
                          writeln('                                                           ')
                        )

>>>>>>> main
                ; clearScreen,
                  writeln('                                                          '),
                  writeln('           |  Atividade inexistente! Tente novamente.  |  '),
                  writeln('                                                          ')
                )

        ; clearScreen,
          writeln('                                                                                                          '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
          writeln('                                                                                                          ')
        ). 


<<<<<<< HEAD
% | Inicia uma atividade, mudando o status para pendente
comecarAtividade :-

        listarAtividades,

        writeln('                                                      '),
        writeln('            |  Começar atividade:  |                  '),
        writeln('                                                      '), nl,
=======
        writeln('                                                        '),
        writeln('            |  Finalizar atividade:  |                  '),
        writeln('                                                        '),
>>>>>>> main

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        write('Digite o ID da atividade que deseja começar: '),
        ler_string(IdAtividade), nl,

<<<<<<< HEAD
        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),
=======
                listarAtividades, nl,

                write('Digite o ID da atividade que deseja começar: '),
                ler_string(IdAtividade), nl,
>>>>>>> main

        (ExisteUsuario, ExisteAtividade ->

                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),

                (Atividade.idMembroResponsavel == IdUsuario ->
                        editarStatusAtividade('Database/bancoDeAtividades.json', IdAtividade, 'Pendente...'),
                        writeln('                                                       '),
                        writeln('        |  Atividade iniciada com sucesso!  |          '),
                        writeln('                                                       ')
                
                ; clearScreen,
                        writeln('                                                           '),
                        writeln('       |  Você não está atribuído a essa atividade!  |     '),
                        writeln('                                                           ')
                )

        ; clearScreen,
          writeln('                                                                                                             '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível iniciar a atividade, tente novamente!  |          '),
          writeln('                                                                                                          ')
        ). 
      

% | Finaliza uma atividade, mudando o status para concluída
finalizarAtividade:-

        listarAtividades,

        writeln('                                                        '),
        writeln('            |  Finalizar atividade:  |                  '),
        writeln('                                                        '), nl,

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        write('Digite o ID da atividade que deseja começar: '),
        ler_string(IdAtividade), nl,

        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteUsuario, ExisteAtividade ->

                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),

                (Atividade.idMembroResponsavel == IdUsuario ->
                        editarStatusAtividade('Database/bancoDeAtividades.json', IdAtividade, 'Concluída!'),
                        clearScreen,
                        writeln('                                                         '),
                        writeln('         |  Atividade concluída com sucesso!  |          '),
                        writeln('                                                         ')
                
                ; clearScreen,
                        writeln('                                                           '),
                        writeln('       |  Você não está atribuído a essa atividade!  |     '),
                        writeln('                                                           ')
                )
        ; clearScreen,
          writeln('                                                                                                          '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível finalizar a atividade, tente novamente!  |          '),
          writeln('                                                                                                          ')
        ).


% | Exibe todas as atividades de um projeto específico
visualizarAtividadesDoProjeto:-

        writeln('                                                        '),
        writeln('        |  Visualizar atividades do projeto:  |         '),
        writeln('                                                        '), nl,

        write('Digite o ID do projeto: '),
        ler_string(IdProjeto), nl,

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/bancoDeAtividades.json', Atividades),

        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        (ExisteProjeto ->

                getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),
                ListaAtividades = Projeto.atividadesAtribuidas,
                length(ListaAtividades, QuantidadeDeAtividades),

                (QuantidadeDeAtividades == 0 -> write('      |     Não há atividades no projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl, nl

                ; write('   |     Estas são as atividades do projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl, nl,
                exibirAtividadesDoProjeto(IdProjeto, ProjetosDoSistema, Atividades)
                )

        ; clearScreen,
        writeln('                                                                   '),
        writeln('               |  Projeto inexistente!  |                          '),
        writeln('                                                                   ')
        ).

% | Exibe o status de uma atividade específica
visualizarStatusAtividade:-

        listarAtividades,

        writeln('                                                          '),
        writeln('           |  Mostrar status da atividade:  |             '),
        writeln('                                                          '),

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),  
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteAtividade -> 
                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
                write('|- Título: '), writeln(Atividade.titulo),
                write('|- Status: '), writeln(Atividade.status)

        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível mostrar o status a atividade, tente novamente!  |          '),
        writeln('                                                                                                          ')
        ).
      

% | Exibe uma atividade específica
consultarAtividade:-

        listarAtividades,

        writeln('                                                          '),
        writeln('                |  Mostrar Atividade:  |                  '),
        writeln('                                                          '), nl,

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),  
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteAtividade -> 
                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
                exibirAtividade(Atividade)

        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível mostrar a atividade, tente novamente!  |        '),
        writeln('                                                                                                          ')
        ).


% | Cria um comentário sobre uma atividade específica
criaFeedback:-

        listarAtividades,

        writeln('                                                                       '),
        writeln('   |  Comente sobre uma atividade que você criou ou foi designado:  |  '),
<<<<<<< HEAD
        writeln('                                                                       '), nl,

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
        IdProjeto = Atividade.idProjetoAtividade,

        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteUsuario, ExisteAtividade, (IdProjeto \= 'Não atribuído!') -> 

                lerJSON('Database/projetos.json', ProjetosDoSistema),
                getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),

                ((Atividade.idMembroResponsavel == IdUsuario ; Projeto.idGerente == IdUsuario) ->

                        write('Escreva um breve comentário sobre a atividade: '),
                        ler_string(Feedback), nl,

                        criarFeedback('Database/bancoDeAtividades.json', Atividade, Feedback),
                        clearScreen,
                        writeln('                                                          '),
                        writeln('   |  Comentário adicionado com sucesso a atividade de ID '), write(IdAtividade), write(' |                  '),
                        writeln('                                                          '),
                        lerJSON('Database/bancoDeAtividades.json', AtividadesAtualizadas),
                        getAtividadeJSON(IdAtividade, AtividadesAtualizadas, AtividadeAtualizada),
                        exibirAtividade(AtividadeAtualizada)

                ; clearScreen,
                        writeln('                                                               '),
                        writeln('    |  Você não está autorizado a realizar esta ação!  |       '),
                        writeln('                                                               '))

        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
        writeln('                                                                                                          ')
=======
        writeln('                                                                       '),

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),

        (ExisteUsuario ->

                write('Digite o ID da atividade: '),
                ler_string(IdAtividade), nl,

                atividadeJaExiste(IdAtividade, AtividadesDoSistema, ExisteAtividade),

                (ExisteAtividade -> % Mesmo existindo não reconhece

                        getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),

                        IdProjeto = Atividade.idProjetoAtividade,

                        lerJSON('Database/projetos.json', ProjetosDoSistema),
                        getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),

                        ((Atividade.idMembroResponsavel == IdUsuario ; Projeto.idGerente == IdUsuario) , IdProjeto \= 'Não atribuído!' ->

                                write('Escreva um breve comentário sobre a atividade: '),
                                ler_string(Feedback), nl,

                                criarFeedback('Database/bancoDeAtividades.json', Atividade, Feedback),
                                writeln('                                                          '),
                                writeln('   |  Comentário adicionado com sucesso a atividade de ID '), write(IdAtividade), write(' |                  '),
                                writeln('                                                          '),
                                lerJSON('Database/bancoDeAtividades.json', AtividadesAtualizadas),
                                getAtividadeJSON(IdAtividade, AtividadesAtualizadas, AtividadeAtualizada),
                                exibirAtividade(AtividadeAtualizada)

                        ; clearScreen,
                          writeln('                                                               '),
                          writeln('    |  Você não está autorizado a realizar esta ação!  |       '),
                          writeln('                                                               '))

                ; clearScreen,
                  writeln('                                                      '),
                  writeln('       |  Atividade inexistente! Tente novamente.  |  '),
                  writeln('                                                      '))

        ; clearScreen,
          writeln('                                                          '),
          writeln('           |  ID incorreto! Tente novamente.  |           '),
          writeln('                                                          ')
>>>>>>> main
        ).
        