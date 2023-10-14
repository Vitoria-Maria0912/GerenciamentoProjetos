

:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, visualizarProjetos/0, deletarProjeto/0, 
          gerenciarMembros/0, menuBancoDeAtividades/0, criarAtividade/0, deletaAtividade/0, alterarIdProjeto/0,
          listarAtividades/0, visualizarStatusAtividade/0, retornoMenuRestrito/0, erroMenuGerente/0]).

% :- discontiguous menuGerente:retornoMenuRestrito/0.

% :- use_module("Menus/MenuGeral.pl").
:- use_module("Controllers/Atividades.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").

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

        % clearScreen,

        ( Entrada == 'l' -> visualizarProjetos, retornoMenuRestrito
        ; Entrada == 'p' -> deletarProjeto
        ; Entrada == 'g' -> gerenciarMembros
        ; Entrada == 'b' -> menuBancoDeAtividades
        ; Entrada == 'm' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).

deletarProjeto :-
        writeln('                                                          '),
        writeln('                 |  Remover Projeto:  |                   '),
        writeln('                                                          '), 
        write('Digite seu Id: '),
        ler_string(IdUsuario), nl,
    
        (nao_vazia(IdUsuario) ->
            lerProjetosJson('Database/projetos.json', ProjetosDoSistema),
            write('Digite o ID do projeto que deseja excluir: '),
            ler_string(IdProjeto), nl,
            
            (nao_vazia(IdProjeto) ->
                verifica_id_projeto(IdProjeto, ProjetosDoSistema, Existe),
                (Existe ->
                    removerProjeto('Database/projetos.json', IdProjeto),
                    writeln(''), nl, retornoMenuRestrito
                ;
                    writeln('O projeto não existe. Tente novamente.'), nl, retornoMenuRestrito
                )
            ;
                writeln('ID do projeto não pode ser vazio. Tente novamente.'), nl, retornoMenuRestrito
            )
        ;
            erroMenuGerente
        ),
        retornoMenuRestrito.

gerenciarMembros :-
        writeln('                                                          '),
        writeln('                                                          '),
        writeln('             |  Gerenciamento de membros:  |              '),
        writeln('                                                          '),

        write('Digite o ID do projeto: '),
        ler_string(IdProjeto), nl,

        % ANTES DE MOSTRAR TEM QUE VERIFICAR SE O PROJETO EXISTE, olhar Haskell

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
        processaEntradaMembros(LowerOption, IdProjeto),
        halt. 

processaEntradaMembros(Entrada, IdProjeto) :- 

        clearScreen,       

        ( Entrada == 'm' -> visualizarMembros(IdProjeto)
        % ; Entrada == 'a' -> atribuirAtividade
        % ; Entrada == 'n' -> adicionarMembro
        % ; Entrada == 'r' -> removerMembro
        ; Entrada == 'p' -> menuRestritoProjeto
        ; Entrada == 'v' -> menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).

visualizarMembros(IdProjeto) :-

        % precisa ser melhorado depois
        writeln('                                                                                     '),
        writeln('   |     Estes são os membros do projeto: (ID '), IdProjeto, write('    |            '),
        writeln('                                                                                     '),

        % imprimeMembrosDoProjeto >>>>>>>> AINDA PRECISA SER FEITO
        retornoMenuRestrito.

adicionaNovoMembro(IdProjeto) :-
        writeln('                                                                    '),
        writeln('                 |     Adicionar novo membro:    |                  '),
        writeln('                                                                   '),
        writeln(' |     Usuários disponíveis no sistema para adição no projeto:    | '),
        writeln('                                                                   '),

        % imprimirUsuario  >>>>>>>> AINDA PRECISA SER FEITO

        write('Digite o ID do membro que deseja adicionar: '),
        ler_string(IdNovoMembro), nl,

        % SE JÁ ESTÁ NO PROJETO
        writeln('                                                                    '),
        writeln('              |     Membro já está no projeto    |                  '),
        writeln('                                                                    '),

        % SE É O GERENTE
        writeln('                                                                    '),
        writeln('          |     O ID pertence ao gerente do projeto!    |           '),
        writeln('                                                                    '),

        % SE DEU CERTO
        writeln('                                                                    '),
        writeln('              |     Membro adicionado com sucesso!    |             '),
        writeln('                                                                    '),

        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                 '),
        writeln('                                                                    '),
        % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        % SE NÃO EXISTE usuário/ atividade /projeto
        writeln('                                                                    '),
        writeln('              |     ID inexistente, tente novamente!    |            '),
        writeln('                                                                    '),

        retornoMenuRestrito.

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

        retornoMenuRestrito.

atribuirMembro(IdProjeto) :-
        writeln('                                                                    '),
        writeln('         |     Atribuir uma atividade a um membro:    |             '),
        writeln('                                                                    '),
        writeln('              |     Atuais membros do projeto:    |                  '),
        writeln('                                                                    '),
        % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        % SÓ O GERENTE PODE ATRIBUIR?

        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,

        write('Digite o ID do membro que deseja atribuir à atividade: '),
        ler_string(IdMembro), nl,

        % SE JÁ ESTÁ ATRIBUÍDA
        writeln('                                                                      '),
        writeln('              |     Atividade já está atribuída!    |                 '),
        writeln('                                                                      '),

        % SE NÃO É MEMBRO
        writeln('                                                                      '),
        writeln('               |     Membro não está no projeto!    |                 '),
        writeln('                                                                      '),

        % SE A ATIVIDADE NÃO TEM ID
        writeln('                                                                      '),
        writeln('           |     A atividade não pertence ao projeto    |             '),
        writeln('                                                                      '),

        % SE DEU CERTO
        writeln('                                                                      '),
        writeln('              |     Atividade atribuída com sucesso!    |             '),
        writeln('                                                                      '),

        % SE NÃO EXISTE usuário/ atividade /projeto
        writeln('                                                                    '),
        writeln('              |     ID inexistente, tente novamente!    |           '),
        writeln('                                                                    '),

        retornoMenuRestrito.


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
        
        ( LowerOption == 'l' -> listarAtividades, retornoMenuRestrito
        ; LowerOption == 'c' -> criaAtividade, alterarIdProjeto
        ; LowerOption == 'r' -> deletaAtividade
        ; LowerOption == 'i' -> comecarAtividade, retornoMenuRestrito
        % ; LowerOption == 'f' -> finalizarAtividade
        % ; LowerOption == 'v' -> visualizarAtividades
        ; LowerOption == 'a' -> visualizarStatusAtividade, retornoMenuRestrito
        % ; LowerOption == 'd' -> consultarAtividade
        % ; LowerOption == 'o' -> criaFeedback
        ; LowerOption == 'm' -> menuPrincipal
        ; LowerOption == 'p' -> menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuGerente ).
        
alterarIdProjeto:-
        % lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),
        
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
                                retornoMenuRestrito
        ; LowerOption == 'n' -> menuBancoDeAtividades
        ; erroMenuGerente).


% Deleta uma atividade do projeto(Inicialmente remove pelo o ID da atividade)    <<<< Falta fazer verificação se é vazio as entradas>>>>>>>>>>>>>>>>>>>
deletaAtividade :-
        writeln('                                                                    '),
        writeln('               |  Deletar atividade de um projeto:  |               '),
        writeln('                                                                    '),
        
        lerUsuariosJson('Database/usuarios.json', UsuariosDoSistema),
        writeln('Digite o Id Usuário:'),
        ler_string(IdUsuario), nl,
        writeln('Digite a senha:'),
        ler_string(Senha), nl,
        (nao_vazia(IdUsuario), nao_vazia(Senha) ->
            (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                write('Digite o ID do projeto que terá a atividade a ser deletada: '),
                ler_string(IdProjeto), nl,
                lerProjetosJson('Database/projetos.json', ProjetosDoSistema),
                (nao_vazia(IdProjeto) ->
                    (verifica_id(IdProjeto, ProjetosDoSistema, Existe) ->
                        % fazer a listagem de atividades do projeto e verificar se tem atividades no projeto.
                        write('Digite o ID da Atividade a ser removida: '),
                        % dependendo da função editarIdProjetoAtividade
                        ler_string(IdAtividade), nl,
                        removerAtividade('Database/bancoDeAtividades.json', IdAtividade),
                        writeln('Atividade removida com sucesso.')
                    ;
                        writeln('ID do projeto não existe. Tente novamente.'), nl,
                    )
                ;
                    writeln('ID do projeto não pode ser vazio. Tente novamente.'), nl,
                )
            ;
                writeln('Senha incorreta. Tente novamente.'), nl,
            )
        ;
            writeln('Campos vazios. Tente novamente.'), nl,
            
        )retornoMenuPrincipal.

    
        %removerAtividade('Database/bancoDeAtividades.json', IdAtividade),                          
listarAtividades :-
        writeln('                                                       '),
        writeln('          |  Listar Atividades cadastradas:  |   '),
        writeln('                                                       '),
        exibirAtividades('Database/bancoDeAtividades.json').

visualizarStatusAtividade:-
        writeln('                                                       '),
        writeln('          |  Visualizar status da atividade  |         '),
        writeln('                                                       '),
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),  
        write('Digite o ID da atividade: '),
        ler_string(IdAtividade), nl,
        (atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                getAtividadeJSON(IdAtividade, AtividadesDoSistema, Atividade),
                write(Atividade)
        ; erroMenuGerente),
        retornoMenuRestrito.     

erroMenuGerente :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuRestrito. % não está chamando???

% Retorna ao menu principal ou sai do sistema
retornoMenuRestrito :- 
        writeln('                                                          '),
        writeln(' | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu Principal                       '),
        writeln('                 P - Menu de projetos                     '),
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),

        (LowerOption == 's' -> 
                sairDoSistema, !
                
        ; LowerOption == 'p' -> 
                clearScreen,
                menuRestritoProjeto

        ; LowerOption == 'm' -> 
                clearScreen,
                consult('Menus/MenuGeral.pl')
        
        ; erroMenuGerente ).

