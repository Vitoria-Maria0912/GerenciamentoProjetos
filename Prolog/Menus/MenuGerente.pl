:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, deletarProjeto/0, 
          gerenciarMembros/0, processaEntradaMembros/2, visualizarMembros/1, adicionaNovoMembro/1,
          removeMembroProjeto/1, atribuirMembro/1, menuBancoDeAtividades/0, deletaAtividade/0, 
          alterarIdProjeto/0, retornoMenuRestrito/0, erroMenuGerente/0]).

:- use_module("Controllers/Atividades.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").
:- use_module("Controllers/Usuario.pl").

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

        ( Entrada == 'l' -> clearScreen, visualizarProjetos, retornoMenuRestrito
        ; Entrada == 'p' -> clearScreen, deletarProjeto
        ; Entrada == 'g' -> clearScreen, gerenciarMembros
        ; Entrada == 'b' -> clearScreen, menuBancoDeAtividades
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).

deletarProjeto :-
        writeln('                                                          '),
        writeln('                 |  Remover Projeto:  |                   '),
        writeln('                                                          '), 
        write('Digite seu Id: '),
        ler_string(IdUsuario), nl,
    
        (nao_vazia(IdUsuario) ->
            lerJSON('Database/projetos.json', ProjetosDoSistema),
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

        verifica_id_projeto(IdProjeto, ProjetosDoSistema, Existe),

        (Existe -> 
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
        
        ;
        erroMenuGerente
        ),
        retornoMenuRestrito.

processaEntradaMembros(Entrada, IdProjeto) :- 

        clearScreen,       

        ( Entrada == 'm' -> visualizarMembros(IdProjeto)
        ; Entrada == 'a' -> atribuirAtividade(IdProjeto)
        ; Entrada == 'n' -> adicionaNovoMembro(IdProjeto)
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
        writeln('                                                                    '),
        writeln(' |     Usuários disponíveis no sistema para adição no projeto:    | '),
        writeln('                                                                    '),

        exibirUsuarios('Database/usuarios.json'),

        write('Digite o ID do membro que deseja adicionar: '),
        ler_string(IdNovoMembro), nl,
        editarMembros('Database/projetos.json', IdProjeto, IdNovoMembro),
        writeln('                                                                    '),
        writeln('              |     Membro adicionado com sucesso!    |             '),
        writeln('                                                                    '),

        retornoMenuRestrito.


        % SE JÁ ESTÁ NO PROJETO
        % writeln('                                                                    '),
        % writeln('              |     Membro já está no projeto    |                  '),
        % writeln('                                                                    '),

        % % SE É O GERENTE
        % writeln('                                                                    '),
        % writeln('          |     O ID pertence ao gerente do projeto!    |           '),
        % writeln('                                                                    '),

        % % SE DEU CERTO
       

        % writeln('                                                                    '),
        % writeln('              |     Atuais membros do projeto:    |                 '),
        % writeln('                                                                    '),
        % % imprimirMembrosDoProjeto  >>>>>>>> AINDA PRECISA SER FEITO

        % % SE NÃO EXISTE usuário/ atividade /projeto
        % writeln('                                                                    '),
        % writeln('              |     ID inexistente, tente novamente!    |            '),
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

        retornoMenuRestrito.

% COLOCAR ISSO EM ATRIBUI MEMBRO
% adicionaAtividade :-
%         writeln('                                                          '),
%         writeln('               |     Alterar nome:    |                 '),
%         writeln('                                                          '),
%         write('Digite seu Id: '),
%         ler_string(IdUsuario), nl,
%         write('Digite a nova Atividade: '),
%         ler_string(Atividade), nl,

%         % FALTA VERIFICAÇÃO SE O ID DA ATIVIDADE EXISTE    
%         (nao_vazia(IdUsuario), nao_vazia(Atividade) ->
%                 lerJSON('Database/usuarios.json', UsuariosDoSistema),
%                 verifica_id(IdUsuario, UsuariosDoSistema, Existe),
%                         (Existe = true ->
%                         editarAtividades('Database/usuarios.json', IdUsuario, Atividade),
%                         write('Sucesso!')
%                         ;
%                         writeln('O usuário não existe. Tente novamente.'), nl, retornoMenuPrincipal
%                         )
%                 ;
%                     erroMenuPrincipal
%                 ).

atribuirAtividade(IdProjeto) :-
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
        
        ( LowerOption == 'l' -> clearScreen, listarAtividades, retornoMenuRestrito
        ; LowerOption == 'c' -> clearScreen, criaAtividade, alterarIdProjeto
        ; LowerOption == 'r' -> clearScreen, deletaAtividade
        ; LowerOption == 'i' -> clearScreen, comecarAtividade, retornoMenuRestrito
        ; LowerOption == 'f' -> clearScreen, finalizarAtividade, retornoMenuRestrito
        ; LowerOption == 'v' -> clearScreen, visualizarAtividades, retornoMenuRestrito
        ; LowerOption == 'a' -> clearScreen, visualizarStatusAtividade, retornoMenuRestrito
        ; LowerOption == 'd' -> clearScreen, consultarAtividade, retornoMenuRestrito
        ; LowerOption == 'o' -> clearScreen, criaFeedback, retornoMenuRestrito
        ; LowerOption == 'm' -> clearScreen, menuPrincipal
        ; LowerOption == 'p' -> clearScreen, menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuGerente ).
        
alterarIdProjeto:-
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
                                retornoMenuRestrito
        ; LowerOption == 'n' -> menuBancoDeAtividades
        ; erroMenuGerente).


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
                                retornoMenuRestrito
                                
                                )   
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
        writeln('                                                               '),
        writeln('      | Deseja voltar ao menu do projeto ou sair do sistema?  |'),
        writeln('                                                               '),
        writeln('                      M - Menu Principal                       '),
        writeln('                      P - Menu de projetos                     '),
        writeln('                      S - Sair do sistema                      '),
        writeln('                                                               '),
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

