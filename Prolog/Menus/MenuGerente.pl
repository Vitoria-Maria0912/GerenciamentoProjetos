:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, deletarProjeto/0, 
          gerenciarMembros/0, processaEntradaMembros/2, visualizarMembros/1, adicionaNovoMembro/1,
          removerMembroProjeto/1, atribuirAtividade/1, menuBancoDeAtividades/0, deletaAtividade/0, 
          addIdProjeto/0, retornoMenuProjetos/0, erroMenuProjeto/0]).


:- use_module("Controllers/Atividades.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").

% | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :-

        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('           L - Listar projetos cadastrados                '), 
        writeln('           P - Remover projeto                            '),  
        writeln('           G - Gerenciar membros do projeto               '),  
        writeln('           B - Menu do banco de atividades                '),  
        writeln('           M - Voltar ao menu principal                   '),
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

deletarProjeto :-

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
        ; Entrada == 'r' -> removerMembroProjeto(IdProjeto)
        ; Entrada == 'p' -> menuRestritoProjeto
        ; Entrada == 'v' -> menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuProjeto ).

% visualizarMembros(IdProjeto) :-

%         lerJSON('Database/projetos.json', ProjetosDoSistema),
%         lerJSON('Database/usuarios.json', Usuarios),

%         getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),
%         ListaMembros = Projeto.membros,
%         length(ListaMembros, QuantidadeDeMembros),

%         (QuantidadeDeMembros == 0 -> nl, write('      |     Não há membros no projeto: (ID: '), write(IdProjeto), write(')    |     '), nl

%         ; nl, write('   |     Estes são os membros do projeto: (ID: '), write(IdProjeto), write(')    |     '), nl,
%         nl, retornarMembros(IdProjeto, ProjetosDoSistema, Membros, Usuarios)
%         ).


%testando -------------------------
visualizarMembros(IdProjeto) :-

        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/usuarios.json', Usuarios),

        getProjetoJSON(IdProjeto, ProjetosDoSistema, Projeto),
        ListaMembros = Projeto.membros,
        length(ListaMembros, QuantidadeDeMembros),

        (QuantidadeDeMembros == 0 -> nl, clearScreen, write('      |     Não há membros no projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl

        ; nl, clearScreen, write('   |     Estes são os membros do projeto: (ID: '), write(IdProjeto), writeln(')    |     '), nl,
        exibirMembros(IdProjeto, ProjetosDoSistema, Usuarios)
        ).
%------------------------------------


% FALTA VERIFICAÇÃO SE É MEMBRO DE UM PROJETO, OU SE JÁ É MEMBRO DO PROJETO
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



        removerMembroProjeto(IdProjeto) :-
                writeln('                                                                    '),
                writeln('              |     Remover membro do projeto:    |                 '),
                writeln('                                                                    '),
                writeln('              |     Atuais membros do projeto:    |                  '),
                writeln('                                                                    '),
                exibirMembros(IdProjeto, ProjetosDoSistema, Usuarios),
                lerJSON('Database/projetos.json', Projetos),
                getProjetoJSON(IdProjeto, Projetos, Projeto),
                write('Digite o ID do membro que deseja remover: '),
                ler_string(IdNovoMembro),
                nl,
                (   nao_vazia(IdNovoMembro) ->
                    verifica_id(IdNovoMembro, Usuarios, Existe),
                    (   Existe ->
                        (   gerenteDoProjeto(IdProjeto, IdNovoMembro, Projetos) ->
                            writeln('                                                                    '),
                            writeln('            |      O ID pertence ao gerente do projeto!    |        '),
                            writeln('                                                                    '),
                            retornoMenuProjetos
                            ;
                            (   not(membroDoProjeto(IdNovoMembro, Projeto)) ->
                                writeln('                                                                    '),
                                writeln('              |     Usuário não é membro do projeto    |            '),
                                writeln('                                                                    '),
                                retornoMenuProjetos
                                ;
                                % Remover membro do projeto
                                removerMembro('Database/projetos.json', IdProjeto, IdNovoMembro),
                                writeln('                                                                    '),
                                writeln('              |     Membro removido com sucesso!    |               '),
                                writeln('                                                                    ')
                            )
                        )
                        ;
                        writeln('                                                                    '),
                        writeln('              |     ID inexistente, tente novamente!    |            '),
                        writeln('                                                                    '),
                        retornoMenuProjetos
                    )
                    ;
                    erroMenuProjeto
                ).
            





% removeMembroProjeto(IdProjeto) :-
%         writeln('                                                                    '),
%         writeln('              |     Remover membro do projeto:    |                 '),
%         writeln('                                                                    '),
%         writeln('              |     Atuais membros do projeto:    |                  '),
%         writeln('                                                                    '),
%         % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

%         write('Digite o ID do membro que deseja remover: '),
%         ler_string(IdNovoMembro), nl,

%         % SE NÃO ESTÁ NO PROJETO
%         writeln('                                                                    '),
%         writeln('              |     Usuário não é membro do projeto    |            '),
%         writeln('                                                                    '),

%         % SE É O GERENTE
%         writeln('                                                                    '),
%         writeln('          |     O ID pertence ao gerente do projeto!    |           '),
%         writeln('                                                                    '),

%         % SE DEU CERTO
%         writeln('                                                                    '),
%         writeln('              |     Membro removido com sucesso!    |               '),
%         writeln('                                                                    '),

%         writeln('              |     Atuais membros do projeto:    |                  '),
%         writeln('                                                                    '),
%         % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

%         % SE NÃO EXISTE usuário/ atividade /projeto
%         writeln('                                                                    '),
%         writeln('              |     ID inexistente, tente novamente!    |            '),
%         writeln('                                                                    '),

%         retornoMenuProjetos.



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


menuBancoDeAtividades :-
        writeln('                                                          '),
        writeln('           |     Menu Banco de Atividades    |            '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('             L - Listar atividades cadastradas            '),
        writeln('             C - Criar uma atividade                      '),
        writeln('             R - Remover uma atividade                    '),
        writeln('             I - Iniciar uma atividade                    '), 
        writeln('             F - Finalizar uma atividade                  '),
        writeln('             V - Visualizar atividades do projeto         '),
        writeln('             A - Visualizar status de uma atividade       '),
        writeln('             D - Consultar uma atividade por ID           '),
        writeln('             O - Dar feedback em uma atividade            '),
        writeln('             P - Voltar ao menu de projetos               '),
        writeln('             M - Voltar ao menu principal                 '),
        writeln('             S - Sair do sistema                          '),
        writeln('                                                          '),

        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        
        ( LowerOption == 'l' -> clearScreen, listarAtividades, retornoMenuProjetos
        ; LowerOption == 'c' -> clearScreen, criaAtividade, addIdProjeto
        % ; LowerOption == 'r' -> clearScreen, deletaAtividade
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
        % lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
        
        writeln('Deseja adicionar a atividade a um projeto? (S/N)'),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption), nl,

        ( LowerOption == 's' -> write('Digite, novamente, o ID da atividade: '),
                                ler_string(IdAtividade), nl,

                                write('Digite o ID do projeto que deseja adicionar a atividade: '),
                                ler_string(IdProjetoAtividade), nl,

                                % É NECESSÁRIA A VERIFICAÇÃO DO PROJETO!!!         <<<<< Não está alterando >>>>>>>>

                                % poderia tirar o retorno, não?
                                editarIdProjetoAtividade('Database/bancoDeAtividades.json', IdAtividade, IdProjetoAtividade),
                                writeln('Atividade alterada com sucesso!'),
                                retornoMenuProjetos
        ; LowerOption == 'n' -> menuBancoDeAtividades
        ; erroMenuProjeto).


% Deleta uma atividade do projeto(Inicialmente remove pelo o ID da atividade)    <<<< Falta fazer verificação se é vazio as entradas>>>>>>>>>>>>>>>>>>>
deletaAtividade :-
        writeln('                                                                    '),
        writeln('               |  Deletar atividade de um projeto:  |               '),
        writeln('                                                                    '),
        
        %write('Digite o ID da Atividade a ser deletada: '),
        %%ler_string(IdAtividade), nl,
        %---- DEVERIA SER ESSA CONFIGURAÇÃO , COM BASE NO ID DO PROJETO PASSADO
        write('Digite o ID do projeto que terá a atividade a ser deletada: '),
        ler_string(IdProjeto), nl,
        % Sem a leitura abaixo não consigo remover
        (nao_vazia(IdProjeto) ->
                        lerJSON('Database/projetos.json', ProjetosDoSistema),
                        lerJSON('Database/bancoDeAtividades.json', AtividadesDoSistema),
                        write('Verificando ID Projeto : '), writeln(IdProjeto), nl,
                        verifica_id(IdProjeto, ProjetosDoSistema, Existe),

                        verifica_id(IdAtividade, AtividadesEncontradas, Presente),
                                (Existe == true , Presente == true ->
                                        
                                        write('Digite o ID da atividade a ser removida'),
                                        getAtividadeJSON(IdProjeto,AtividadesDoSistema,AtividadesEncontradas),
                                        exibirAtividadesAux(AtividadesEncontradas),
                                        ler_string(IdAtividade), nl,
                                        removerAtividade('Database/bancoDeAtividades.json', IdAtividade), 
                                    
                                        %% listar atividades do projeto e depois escolhe e remove 
                                
                                writeln('O usuário não existe. Tente novamente.'), 
                                retornoMenuProjetos
                                
                                )   
        ; erroMenuProjeto),

       retornoMenuProjetos.

