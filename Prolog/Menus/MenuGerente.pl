:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, deletarProjeto/0, 
          gerenciarMembros/0, processaEntradaMembros/2, visualizarMembros/1, adicionaNovoMembro/1,
          removeMembroProjeto/1, atribuirAtividade/1, menuBancoDeAtividades/0, deletaAtividade/0, 
          addIdProjeto/0, retornoMenuProjetos/0, erroMenuProjeto/0]).

:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Atividades.pl").

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
        downcase_atom(Input, Entrada),

        ( Entrada == 'l' -> clearScreen, visualizarProjetos
        ; Entrada == 'p' -> clearScreen, deletarProjeto
        ; Entrada == 'g' -> clearScreen, gerenciarMembros
        ; Entrada == 'b' -> clearScreen, menuBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ),

        retornoMenuProjetos.

% | Exclui um projeto do sistema
deletarProjeto :- 

        visualizarProjetos,

        writeln('                                                          '),
        writeln('                 |  Remover Projeto:  |                   '),
        writeln('                                                          '), nl,

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,
        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        write('Digite o ID do projeto que deseja excluir: '),
        ler_string(IdProjeto), nl,
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        (ExisteUsuario, ExisteProjeto -> 

                write('Digite sua senha: '),
                ler_string(Senha), nl,

                getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),

                (Projeto.idGerente == IdUsuario, verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
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
  

% | Menu de gerenciamento de membros de um projeto
gerenciarMembros :-
        writeln('                                                          '),
        writeln('                                                          '),
        writeln('             |  Gerenciamento de membros:  |              '),
        writeln('                                                          '), nl,

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        write('Digite o ID do projeto: '),
        ler_string(IdProjeto), nl,

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, ExisteProjeto),

        getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),

        (ExisteProjeto, Projeto.idGerente == IdUsuario -> 
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
                downcase_atom(Input, Entrada),
                
                ( Entrada == 'm' -> clearScreen, visualizarMembros(IdProjeto)
                ; Entrada == 'a' -> clearScreen, atribuirAtividade(IdProjeto)
                ; Entrada == 'n' -> clearScreen, adicionaNovoMembro(IdProjeto)
                ; Entrada == 'r' -> clearScreen, removeMembroProjeto(IdProjeto)
                ; Entrada == 'p' -> clearScreen, menuRestritoProjeto
                ; Entrada == 'v' -> clearScreen, menuPrincipal
                ; Entrada == 's' -> sairDoSistema
                ; erroMenuProjeto)
        
        ; clearScreen,
        writeln('                                                                                                                        '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível gerenciar os membros do projeto, tente novamente!  |          '),
        writeln('                                                                                                                        ')

        ), retornoMenuProjetos.
% | Exibe todos os membros de um projeto específico
visualizarMembros(IdProjeto) :-

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/usuarios.json', Usuarios),

        getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),
        ListaMembros = Projeto.membros,
        length(ListaMembros, QuantidadeDeMembros),

        (QuantidadeDeMembros == 0 -> clearScreen, write('      |     Não há membros no projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl, nl

        ; write('   |     Estes são os membros do projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl,nl,
        exibirMembros(IdProjeto, ProjetosDoSistema, Usuarios)
        ).

% | Atribui a atividade a um membro, tornando-o membro responsável por ela
atribuirAtividade(IdProjeto) :-
        writeln('                                                                    '),
        writeln('         |     Atribuir uma atividade a um membro:    |             '),
        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),
       
        visualizarMembros(IdProjeto),

        lerJSON('Database/projetos.json', Projetos),
        lerJSON('Database/usuarios.json', Usuarios),
        lerJSON('Database/bancoDeAtividades.json', Atividades),
        getProjetoJSON(IdProjeto, Projetos, Projeto),

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        (nao_vazia(IdAtividade) ->
        verifica_id_atividade(IdAtividade, Atividades, AtvExiste),
        (AtvExiste ->
        jaAtribuida(IdAtividade, Projeto) ->
        writeln('                                                          '),
        writeln('           |     A atividade já está atribuída!    |      '),
        writeln('                                                          '), nl, retornoMenuProjetos;
        write('Digite o ID do membro que deseja atribuir à atividade: '),
        ler_string(IdMembro), nl,
        (nao_vazia(IdMembro) ->
        verifica_id(IdMembro, Usuarios, Existe),
        (Existe ->
                membroDoProjeto(IdMembro, Projeto) ->
                editarAtividades('Database/usuarios.json', IdMembro, IdAtividade),
                addAtividadesProj('Database/projetos.json', IdProjeto, IdAtividade),
                editarMembroResponsavelAtividade('Database/bancoDeAtividades.json', IdAtividade, IdMembro),
                writeln('                                                                      '),
                writeln('              |     Atividade atribuída com sucesso!    |             '),
                writeln('                                                                      '), nl, retornoMenuProjetos
                ; 
                writeln('                                                                      '),
                writeln('              |     Membro não está no projeto!         |             '),
                writeln('                                                                      '), nl, retornoMenuProjetos
        ;
        writeln('                                                                    '),
        writeln('              |     ID inexistente, tente novamente!    |           '),
        writeln('                                                                    '), nl, retornoMenuProjetos  
        )
        ; 
        erroMenuProjeto, retornoMenuProjetos
        );  
        writeln('                                                          '),
        writeln('           |     A atividade não existe!    |             '),
        writeln('                                                          '), nl, retornoMenuProjetos 
        );
        erroMenuProjeto, retornoMenuProjetos
        ).


% | Adiciona um novo membro a um projeto específico
adicionaNovoMembro(IdProjeto) :-
        writeln('                                                                    '),
        writeln('                 |     Adicionar novo membro:    |                  '),
        writeln('                                                                    '),
        writeln(' |     Usuários disponíveis no sistema para adição no projeto:    | '),
        writeln('                                                                    '),

        exibirUsuarios('Database/usuarios.json'),
        lerJSON('Database/usuarios.json', Usuarios),
        lerJSON('Database/projetos.json', Projetos),
        getProjetoJSON(IdProjeto, Projetos, Projeto),

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
                (membroDoProjeto(IdNovoMembro, Projeto) ->
                    writeln('                                                                    '),
                    writeln('        |     O membro já pertence ao projeto!    |                 '),
                    writeln('                                                                    '),
                    retornoMenuProjetos
                ;
                    editarMembros('Database/projetos.json', IdProjeto, IdNovoMembro),
                    writeln('                                                                    '),
                    writeln('              |     Membro adicionado com sucesso!    |             '),
                    writeln('                                                                    '),
                   retornoMenuProjetos
                )
                )
                ; 
                writeln('                                                                    '),
                writeln('              |     ID inexistente, tente novamente!    |           '),
                writeln('                                                                    '),
                retornoMenuProjetos
        ) ; erroMenuProjeto
        ).

        ; clearScreen,
        writeln('                                                                                                                        '),
        writeln(' |  Campo obrigatório vazio ou inválido, não foi possível adicionar o membro ao projeto, tente novamente!  |          '),
        writeln('                                                                                                                        ')
        
        ).

removeMembroProjeto(IdProjeto) :-
        writeln('                                                                    '),
        writeln('              |     Remover membro do projeto:    |                 '),
        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),

        write('Digite o ID do membro que deseja remover: '),
        ler_string(IdMembro), nl,

        lerJSON('Database/projetos.json', Projetos),
        removerMembro('Database/projetos.json', IdProjeto, IdMembro),
        
        % exibirMembros(IdProjeto, ProjetosDoSistema, Usuarios),

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
        writeln('                                                                    ').
        

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
        
        ( LowerOption == 'l' -> clearScreen, listarAtividades
        ; LowerOption == 'c' -> clearScreen, criaAtividade, addIdProjeto
        ; LowerOption == 'r' -> clearScreen, deletaAtividade
        ; LowerOption == 'i' -> clearScreen, comecarAtividade
        ; LowerOption == 'f' -> clearScreen, finalizarAtividade
        ; LowerOption == 'v' -> clearScreen, visualizarAtividadesDoProjeto
        ; LowerOption == 'a' -> clearScreen, visualizarStatusAtividade
        ; LowerOption == 'd' -> clearScreen, consultarAtividade
        ; LowerOption == 'o' -> clearScreen, criaFeedback
        ; LowerOption == 'm' -> clearScreen, menuPrincipal
        ; LowerOption == 'p' -> clearScreen, menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuProjeto),
        
        retornoMenuProjetos.
        
% | Adiciona uma atividade a um projeto
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
        ; erroMenuProjeto).


% | Deleta uma atividade do projeto, mas não do banco de atividades(a tendência é que ele cresça)
deletaAtividade :-                

        visualizarProjetos,

        writeln('                                                                    '),
        writeln('               |  Deletar atividade de um projeto:  |               '),
        writeln('                                                                    '), nl,

        write('Digite o ID da atividade a ser deletada: '),
        ler_string(IdAtividade), nl,
        write('Digite o ID do projeto que a atividade pertence: '),
        ler_string(IdProjetoAtividade), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),

        verifica_id_projeto(IdProjetoAtividade, ProjetosDoSistema, ExisteProjeto),
        verifica_id_atividade(IdAtividade, AtividadesDoSistema, ExisteAtividade),

        (ExisteProjeto, ExisteAtividade -> 

                write('Digite seu ID: '),
                ler_string(IdUsuario), nl,

                write('Digite sua senha: '),
                ler_string(Senha), nl,
                
                verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                getProjetoJSON(IdProjetoAtividade, ProjetosDoSistema, Projeto),

                (ExisteUsuario, Projeto.idGerente == IdUsuario, verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->

                        removerAtividadesProjeto('Database/bancoDeAtividades.json', IdProjetoAtividade, IdAtividade),
                        editarIdProjetoAtividade('Database/bancoDeAtividades.json', IdAtividade, 'Não atribuído!'),
                        writeln('                                                    '),
                        writeln('      |  Atividade deletada do projeto com sucesso!  |   '),
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

        ). 