

:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, visualizarProjetos/0, deletarProjeto/0, 
          gerenciarMembros/0, menuBancoDeAtividades/0, criaAtividade/0, deletaAtividade/0, alterarIdProjeto/1,
          listarAtividades/0, visualizarStatusAtividade/0, retornoMenuRestrito/0, erroMenuGerente/0]).

% :- discontiguous menuGerente:retornoMenuRestrito/0.

% :- use_module("Menus/MenuGeral.pl").
:- use_module("Controllers/Atividades.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").

% | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :-

        writeln('                                                          '),
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

        % clearScreen,

        ( Entrada == 'l' -> visualizarProjetos
        ; Entrada == 'p' -> deletarProjeto
        ; Entrada == 'g' -> gerenciarMembros
        ; Entrada == 'b' -> menuBancoDeAtividades
        % ; Entrada == 'i' -> comecarAtividade
        % ; Entrada == 'v' -> visualizarAtividades
        ; Entrada == 'a' -> visualizarStatusAtividade
        % ; Entrada == 'o' -> criaFeedback
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
            erroMenuGeral
        ),
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

        ( 
        % Entrada == 'm' -> vizualizarMembros
        % ; Entrada == 'a' -> atribuirAtividade
        % ; Entrada == 'n' -> adicionarMembro
        % ; Entrada == 'r' -> removerMembro
          Entrada == 'p' -> menuRestritoProjeto
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
        
        ( LowerOption == 'c' -> criaAtividade
        ; LowerOption == 'r' -> deletaAtividade
        ; LowerOption == 'l' -> listarAtividades
        % ; LowerOption == 'a' -> consultarAtividade
        ; LowerOption == 'v' -> menuPrincipal
        ; LowerOption == 'p' -> menuRestritoProjeto
        ; LowerOption == 's' -> sairDoSistema
        ; erroMenuGerente ).
        
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
          retornoMenuRestrito
        ), alterarIdProjeto(IdAtividade).

alterarIdProjeto(IdAtividade):-
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),

        writeln('Deseja adicionar a atividade a um projeto? (S/N)'),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption), nl,

        ( LowerOption == 's' -> write('Digite o ID do projeto que deseja adicionar a atividade: '),
                                read_string(user_input, "\n", "\r", _, IdProjetoAtividade), nl,

                                % É NECESSÁRIA A VERIFICAÇÃO DO PROJETO!!!         <<<<< Não está alterando >>>>>>>>

                                % poderia tirar o retorno, não?
                                editarIdProjetoAtividadeJSON(AtividadesDoSistema, IdAtividade, IdProjetoAtividade, _),
                                writeln('Atividade alterada com sucesso!'),
                                retornoMenuRestrito
        ; LowerOption == 'n' -> menuBancoDeAtividades
        ; erroMenuGerente).


                

% Deleta uma atividade do projeto(Inicialmente remove pelo o ID da atividade)    <<<< Falta fazer verificação se é vazio as entradas>>>>>>>>>>>>>>>>>>>
deletaAtividade :-
        writeln('                                                       '),
        writeln('               |  Deletar atividade:  |                '),
        writeln('                                                       '),
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
                        write('Digite o ID da Atividade a ser removida: '),
                        % Aqui você pode adicionar a listagem das atividades no projeto
                        % dependendo da função editarIdProjetoAtividade
                        ler_string(IdAtividade), nl,
                        removerAtividade('Database/bancoDeAtividades.json', IdAtividade),
                        writeln('Atividade removida com sucesso.'),
                        retornoMenuRestrito
                    ;
                        writeln('ID do projeto não existe. Tente novamente.'), nl,
                        retornoMenuRestrito
                    )
                ;
                    writeln('ID do projeto não pode ser vazio. Tente novamente.'), nl,
                    retornoMenuPrincipal
                )
            ;
                writeln('Senha incorreta. Tente novamente.'), nl,
                retornoMenuPrincipal
            )
        ;
            writeln('Campos vazios. Tente novamente.'), nl,
            retornoMenuPrincipal
        ).

    
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
        % clearScreen,
        % writeln('                                                          '),
        % writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        % writeln('                                                          '),
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

