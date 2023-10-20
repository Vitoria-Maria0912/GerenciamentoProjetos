:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, deletarProjeto/0, 
          gerenciarMembros/0, processaEntradaMembros/2, visualizarMembros/1, adicionaNovoMembro/1,
          removeMembroProjeto/1, atribuirAtividade/1, menuBancoDeAtividades/0, deletaAtividade/0, 
          addIdProjeto/0, retornoMenuProjetos/0, erroMenuProjeto/0]).

:- use_module("Controllers/Atividades.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").

% | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :-

        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '), nl,
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '), nl,
        writeln('                                                          '),
        writeln('           L - Listar projetos cadastrados                '), nl, 
        writeln('           P - Remover projeto                            '), nl,  
        writeln('           G - Gerenciar membros do projeto               '), nl,  
        writeln('           B - Menu do banco de atividades                '), nl,  
        writeln('           M - Voltar ao menu principal                   '), nl,
        writeln('           S - Sair do sistema                            '), 
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII),
        downcase_atom(Input, LowerOption),
        processaEntradaMenuRestrito(LowerOption),
        halt. 

processaEntradaMenuRestrito(Entrada) :- 

        ( Entrada == 'l' -> clearScreen, visualizarProjetos, retornoMenuProjetos
        ; Entrada == 'p' -> clearScreen, deletarProjeto
        ; Entrada == 'g' -> clearScreen, gerenciarMembros
        ; Entrada == 'b' -> clearScreen, menuBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ).

deletarProjeto :-    % seria uma boa tambem colocar a senha

        visualizarProjetos,

        writeln('                                                          '),
        writeln('                 |  Remover Projeto:  |                   '),
        writeln('                                                          '), 

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,
        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        write('Digite o ID do projeto que deseja excluir: '),
        ler_string(IdProjeto), nl,
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        (ExisteUsuario, ExisteProjeto -> 

                getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),

                (Projeto.idGerente == IdUsuario ->
                    removerProjeto('Database/projetos.json', IdProjeto)
                   
                ; clearScreen,
                writeln('                                                              '),
                writeln('    |  Você não está autorizado a realizar esta ação !  |     '),
                writeln('                                                              ')
                )

        ; clearScreen,
        writeln('                                                                                                             '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível remover o projeto, tente novamente!  |          '),
        writeln('                                                                                                          ')
        ), retornoMenuProjetos. 
  

gerenciarMembros :-                     % Prolog está frescando no retornoMenuProjetos, não printa 
        writeln('                                                          '),
        writeln('                                                          '),
        writeln('             |  Gerenciamento de membros:  |              '),
        writeln('                                                          '),

        write('Digite o ID do projeto: '),
        ler_string(IdProjeto), nl,

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        (ExisteProjeto -> 
                writeln('                                                          '),
                writeln('                O que deseja fazer agora?                 '), nl,
                writeln('                                                          '),
                writeln('                  Selecione uma opção:                    '), nl,
                writeln('                                                          '),
                writeln('            M - Visualizar membros do projeto             '), nl,
                writeln('            A - Atribuir atividade a um membro            '), nl,
                writeln('            N - Adicionar membro ao projeto               '), nl,
                writeln('            R - Remover membro do projeto                 '), nl,
                writeln('            V - Voltar ao menu principal                  '), nl,
                writeln('            P - Voltar ao menu de projetos                '), nl,
                writeln('            S - Sair do sistema                           '),
                writeln('                                                          '), 

                get_single_char(CodigoASCII),
                char_code(Input, CodigoASCII),
                downcase_atom(Input, LowerOption),
                processaEntradaMembros(LowerOption, IdProjeto)
        
        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
        writeln('                                                                                                          ')

        ), retornoMenuProjetos.

processaEntradaMembros(Entrada, IdProjeto) :- 

        clearScreen,       

        ( Entrada == 'm' -> visualizarMembros(IdProjeto)
        ; Entrada == 'a' -> atribuirAtividade(IdProjeto)
        ; Entrada == 'n' -> adicionaNovoMembro(IdProjeto)
        % ; Entrada == 'r' -> removerMembro
        ; Entrada == 'p' -> menuRestritoProjeto
        ; Entrada == 'v' -> menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ).

visualizarMembros(IdProjeto) :-

        % precisa ser melhorado depois
        writeln('                                                                                     '),
        writeln('   |     Estes são os membros do projeto: (ID '), IdProjeto, write('    |            '),
        writeln('                                                                                     '),
        % imprimeMembrosDoProjeto >>>>>>>> AINDA PRECISA SER FEITO
        retornoMenuProjetos.


adicionaNovoMembro(IdProjeto) :-
        writeln('                                                                    '),
        writeln('                 |     Adicionar novo membro:    |                  '),
        writeln('                                                                    '),
        writeln(' |     Usuários disponíveis no sistema para adição no projeto:    | '),
        writeln('                                                                    '),

        exibirUsuarios('Database/usuarios.json'),
        lerJSON('Database/usuarios.json', Usuarios),
        lerJSON('Database/projetos.json', Projetos),

        write('Digite o ID do membro que deseja adicionar: '),
        ler_string(IdNovoMembro), nl,
        (nao_vazia(IdNovoMembro) ->
        verifica_id(IdNovoMembro, Usuarios, Existe),
        (Existe ->
                (gerenteDoProjeto(IdProjeto, IdNovoMembro, Projetos) ->
                writeln('                                                                    '),
                writeln('            |      O ID pertence ao gerente do projeto!    |        '),
                writeln('                                                                    '), 
                retornoMenuProjetos
                ; 
                editarMembros('Database/projetos.json', IdProjeto, IdNovoMembro),
                writeln('                                                                    '),
                writeln('              |     Membro adicionado com sucesso!    |             '),
                writeln('                                                                    '),
                retornoMenuProjetos
                )
                ; 
                writeln('                                                                    '),
                writeln('              |     ID inexistente, tente novamente!    |           '),
                writeln('                                                                    '),
                retornoMenuProjetos
        ) ; erroMenuProjeto
        );
        
        retornoMenuProjetos.


        % SE JÁ ESTÁ NO PROJETO
        % writeln('                                                                    '),
        % writeln('              |     Membro já está no projeto    |                  '),
        % writeln('                                                                    '),


removeMembroProjeto(IdProjeto) :-
        writeln('                                                                    '),
        writeln('              |     Remover membro do projeto:    |                 '),
        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),
        % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        write('Digite o ID do membro que deseja remover: '),
        ler_string(IdNovoMembro), nl,

        % SE NÃO ESTÁ NO PROJETO
        writeln('                                                                    '),
        writeln('              |     Usuário não é membro do projeto    |            '),
        writeln('                                                                    '),

        % SE É O GERENTE
        writeln('                                                                    '),
        writeln('          |     O ID pertence ao gerente do projeto!    |           '),
        writeln('                                                                    '),

        % SE DEU CERTO
        writeln('                                                                    '),
        writeln('              |     Membro removido com sucesso!    |               '),
        writeln('                                                                    '),

        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),
        % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        % SE NÃO EXISTE usuário/ atividade /projeto
        writeln('                                                                    '),
        writeln('              |     ID inexistente, tente novamente!    |            '),
        writeln('                                                                    '),

        retornoMenuProjetos.

atribuirAtividade(IdProjeto) :-
        writeln('                                                                    '),
        writeln('         |     Atribuir uma atividade a um membro:    |             '),
        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),
        % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        lerJSON('Database/projetos.json', Projetos),
        lerJSON('Database/usuarios.json', Usuarios),
        lerJSON('Database/bancoDeAtividades.json', Atividades),

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        (nao_vazia(IdAtividade) ->
        verifica_id_atividade(IdAtividade, Atividades, AtvExiste),
        (AtvExiste ->
        write('Digite o ID do membro que deseja atribuir à atividade: '),
        ler_string(IdMembro), nl,
        (nao_vazia(IdMembro) ->
        verifica_id(IdMembro, Usuarios, Existe),
        (Existe ->
        editarAtividades('Database/usuarios.json', IdMembro, IdAtividade),
        addAtividadesProjeto('Database/projetos.json', IdProjeto, IdAtividade),
        writeln('                                                                      '),
        writeln('              |     Atividade atribuída com sucesso!    |             '),
        writeln('                                                                      '), nl, retornoMenuProjetos
        ;
        writeln('                                                                    '),
        writeln('              |     ID inexistente, tente novamente!    |           '),
        writeln('                                                                    '), nl, retornoMenuProjetos  
        ); 
        erroMenuProjeto, retornoMenuProjetos
        );  
        writeln('                                                          '),
        writeln('           |     A atividade não existe!    |             '),
        writeln('                                                          '), nl, retornoMenuProjetos 
        );
        erroMenuProjeto, retornoMenuProjetos
        ).


menuBancoDeAtividades :-
        writeln('                                                          '),
        writeln('           |     Menu Banco de Atividades    |            '), nl,
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '), nl,
        writeln('                                                          '),
        writeln('             L - Listar atividades cadastradas            '), nl,
        writeln('             C - Criar uma atividade                      '), nl,
        writeln('             R - Remover uma atividade                    '), nl,
        writeln('             I - Iniciar uma atividade                    '), nl, 
        writeln('             F - Finalizar uma atividade                  '), nl,
        writeln('             V - Visualizar atividades do projeto         '), nl,
        writeln('             A - Visualizar status de uma atividade       '), nl,
        writeln('             D - Consultar uma atividade por ID           '), nl,
        writeln('             O - Dar feedback em uma atividade            '), nl,
        writeln('             P - Voltar ao menu de projetos               '), nl,
        writeln('             M - Voltar ao menu principal                 '), nl,
        writeln('             S - Sair do sistema                          '),
        writeln('                                                          '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        
        ( LowerOption == 'l' -> clearScreen, listarAtividades, retornoMenuProjetos
        ; LowerOption == 'c' -> clearScreen, criaAtividade, addIdProjeto
        ; LowerOption == 'r' -> clearScreen, deletaAtividade
        ; LowerOption == 'i' -> clearScreen, comecarAtividade
        ; LowerOption == 'f' -> clearScreen, finalizarAtividade
        % ; LowerOption == 'v' -> clearScreen, visualizarAtividades, retornoMenuProjetos
        ; LowerOption == 'a' -> clearScreen, visualizarStatusAtividade
        ; LowerOption == 'd' -> clearScreen, consultarAtividade
        ; LowerOption == 'o' -> clearScreen, criaFeedback
        ; LowerOption == 'm' -> clearScreen, menuPrincipal
        ; LowerOption == 'p' -> clearScreen, menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuProjeto ).
        
addIdProjeto:-

        writeln('Deseja adicionar a atividade a um projeto? (S/N)'),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption), nl,

        ( LowerOption == 's' -> visualizarProjetos, nl,
                
                                write('Digite, novamente, o ID da atividade: '),
                                ler_string(IdAtividade), nl,

                                write('Digite o ID do projeto que deseja adicionar a atividade: '),
                                ler_string(IdProjetoAtividade), nl,

                                lerJSON('Database/projetos.json', ProjetosDoSistema),

                                verifica_id_projeto(IdProjetoAtividade, ProjetosDoSistema, ExisteProjeto),
                                verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

                                (ExisteProjeto, ExisteAtividade -> 

                                        write('Digite seu ID: '),
                                        ler_string(IdUsuario), nl,
                                
                                        lerJSON('Database/usuarios.json', UsuariosDoSistema),
                                        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                        
                                        getProjetoJSON(IdProjetoAtividade, ProjetosDoSistema, Projeto),
                        
                                        (ExisteUsuario, Projeto.idGerente == IdUsuario ->

                                                editarIdProjetoAtividade('Database/bancoDeAtividades.json', IdAtividade, IdProjetoAtividade),
                                                addAtividadesProjeto('Database/bancoDeAtividades.json', IdProjetoAtividade, IdAtividade),
                                                writeln('                                                    '),
                                                writeln('            |  Atividade alterada com sucesso!  |   '),
                                                writeln('                                                    ')

                                        ; clearScreen,
                                        writeln('                                                               '),
                                        writeln('    |  Você não está autorizado a realizar esta ação!  |       '),
                                        writeln('                                                               ')
                                        )
                                ; clearScreen,
                                writeln('                                                                                                          '),
                                writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
                                writeln('                                                                                                          ')
                                )
        ; LowerOption == 'n' -> menuBancoDeAtividades
        ; erroMenuProjeto), retornoMenuProjetos.


% Deleta uma atividade do projeto
deletaAtividade :-                

        visualizarProjetos,

        writeln('                                                                    '),
        writeln('               |  Deletar atividade de um projeto:  |               '),
        writeln('                                                                    '),

        write('Digite o ID da atividade a ser deletada: '),
        ler_string(IdAtividade), nl,
        write('Digite o ID do projeto que a atividade pertence: '),
        ler_string(IdProjetoAtividade), nl,

        % visualizarAtividadesDoProjeto(IdProjetoAtividade), nl,     cadê????

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),

        verifica_id_projeto(IdProjetoAtividade, ProjetosDoSistema, ExisteProjeto),
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteProjeto, ExisteAtividade -> 

                write('Digite seu ID: '),
                ler_string(IdUsuario), nl,
                
                verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                getProjetoJSON(IdProjetoAtividade, ProjetosDoSistema, Projeto),

                (ExisteUsuario, Projeto.idGerente == IdUsuario ->

                        editarIdProjetoAtividade('Database/bancoDeAtividades.json', IdAtividade, 'Não atribuído!'),
                        writeln('                                                    '),
                        writeln('        |  Atividade deletada do projeto com sucesso!  |   '),
                        writeln('                                                    ')

                ; clearScreen,
                writeln('                                                               '),
                writeln('    |  Você não está autorizado a realizar esta ação!  |       '),
                writeln('                                                               ')
                )
        ; clearScreen,
        writeln('                                                                                                          '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível deletar a atividade, tente novamente!  |          '),
        writeln('                                                                                                          ')
        ), retornoMenuProjetos.
