:- module(menuPublico, [menuPublicoProjeto/0, processaEntradaMenuPublico/1, visualizarProjetos/0, erroMenuProjeto/0, 
                        retornoMenuProjetos/0 , menuPublicoBancoDeAtividades/0 , processaEntradaBancoDeAtividades/1,
                        criaAtividade/0, listarAtividades/0, comecarAtividade/0, finalizarAtividade/0, visualizarStatusAtividade/0,
                        consultarAtividade/0, criaFeedback/0, visualizarAtividadesDoProjeto/0]).

:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Atividades.pl").
:- use_module("Controllers/Utils.pl").

erroMenuProjeto :-
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

        ; LowerOption == 'p' -> clearScreen, menuProjetos

        ; LowerOption == 'm' -> clearScreen, menuPrincipal
        
        ; erroMenuProjeto ).

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
        downcase_atom(Input, LowerOption),
        processaEntradaMenuPublico(LowerOption),
        halt. 

processaEntradaMenuPublico(Entrada) :- 

        ( Entrada == 'l' -> clearScreen, visualizarProjetos, retornoMenuProjetos
        ; Entrada == 'b' -> clearScreen, menuPublicoBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ).


visualizarProjetos :-
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        exibirProjetos('Database/projetos.json').

% Menu do banco
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
        downcase_atom(Input, LowerOption),
        processaEntradaBancoDeAtividades(LowerOption),
        retornoMenuProjetos. 
        

processaEntradaBancoDeAtividades(Entrada) :- 

        ( Entrada == 'l' -> clearScreen, listarAtividades, retornoMenuProjetos
        ; Entrada == 'c' -> clearScreen, criaAtividade
        ; Entrada == 'i' -> clearScreen, comecarAtividade
        ; Entrada == 'f' -> clearScreen, finalizarAtividade
        ; Entrada == 'v' -> clearScreen, visualizarAtividadesDoProjeto, retornoMenuProjetos
        ; Entrada == 'a' -> clearScreen, visualizarStatusAtividade
        ; Entrada == 'd' -> clearScreen, consultarAtividade
        ; Entrada == 'o' -> clearScreen, criaFeedback
        ; Entrada == 'm' -> clearScreen, menuPrincipal 
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ).

listarAtividades :-
        writeln('                                                '),
        writeln('          |  Atividades cadastradas:  |         '),
        writeln('                                                '),
        exibirAtividades('Database/bancoDeAtividades.json').


criaAtividade :- 

        listarAtividades,

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
                lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
                verifica_id_atividade(IdAtividade, AtividadesDoSistema, Existe),
                (\+ Existe -> 
                        salvarAtividade('Database/bancoDeAtividades.json', Titulo, Descricao, Dificuldade, IdAtividade, 'Não atribuída!', 'Não atribuído!', 'Não atribuído!', []), 
                        write('Atividade criada! E o ID dela é: '), writeln(IdAtividade), nl
                        
                ; clearScreen,
                  writeln('                                                          '),
                  writeln('           |  Atividade inexistente! Tente novamente.  |  '),
                  writeln('                                                          '),
                  retornoMenuProjetos
                )

        ; clearScreen,
          writeln('                                                                                                          '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
          writeln('                                                                                                          '),
          retornoMenuProjetos
        ). 


comecarAtividade :-

        listarAtividades,

        writeln('                                                      '),
        writeln('            |  Começar atividade:  |                  '),
        writeln('                                                      '),

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
        ), retornoMenuProjetos. 
      

finalizarAtividade:-

        listarAtividades,

        writeln('                                                        '),
        writeln('            |  Finalizar atividade:  |                  '),
        writeln('                                                        '),

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
        ), retornoMenuProjetos.


visualizarAtividadesDoProjeto:-

        writeln('                                                        '),
        writeln('        |  Visualizar atividades do projeto:  |         '),
        writeln('                                                        '),

        write('Digite o ID do projeto: '),
        ler_string(IdProjeto), nl,

        lerJSON('Database/projetos.json', ProjetosDoSistema),

        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        (ExisteProjeto ->

                getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),
                ListaAtividades = Projeto.atividadesAtribuidas,
                length(ListaAtividades, QuantidadeDeAtividades),

                (QuantidadeDeAtividades == 0 -> nl, write('      |     Não há atividades no projeto: (ID: '), write(IdProjeto), write(')    |     '), nl

                ; writeln('                                                                       '),
                write('   |     Estas são as atividades do projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl,
                exibirAtividadesDoProjeto(IdProjeto, ProjetosDoSistema, Atividades)
                )

        ; clearScreen,
        writeln('                                                                   '),
        writeln('               |  Projeto inexistente!  |                          '),
        writeln('                                                                   ')
        ).

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
                exibirAtividade(Atividade)

        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível mostrar o status a atividade, tente novamente!  |          '),
        writeln('                                                                                                          ')
        ), retornoMenuProjetos.
      

consultarAtividade:-

        listarAtividades,

        writeln('                                                          '),
        writeln('                |  Mostrar Atividade:  |                  '),
        writeln('                                                          '),

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
        ), retornoMenuProjetos.


criaFeedback:-

        listarAtividades,

        writeln('                                                                       '),
        writeln('   |  Comente sobre uma atividade que você criou ou foi designado:  |  '),
        writeln('                                                                       '),

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
        ), retornoMenuProjetos.
        