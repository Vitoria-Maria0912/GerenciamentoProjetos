:- module(menuGeral, [menuPrincipal/0, processaEntradaMenuPrincipal/1, cadastrarUsuario/0, deletarUsuario/0, cadastrarProjeto/0,
                      menuProjetos/0, menuChat/0, enviarMPrivada/0, enviarMGeral/0, visualizarMensagensPrivadas/0,
                      visualizarMensagensGerais/0, erroMenuPrincipal/0, erroMenuChat/0, menuChat/0, processaEntradaMenuChat/1,
                      retornoMenuPrincipal/0]).

:- initialization(menuPrincipal).
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Mensagem.pl").
:- use_module("Menus/MenuGerente.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").

% Menu principal com as principais funcionalidades
menuPrincipal :-

        writeln('                                                          '),
        writeln('             |     Menu Principal    |                    '), nl,
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '), nl,
        writeln('                                                          '),
        writeln('            C - Cadastrar novo usuário                    '), nl,
        writeln('            D - Deletar perfil                            '), nl,
        writeln('            P - Criar projeto                             '), nl,
        writeln('            G - Menu de projetos                          '), nl,
        writeln('            M - Caixa de mensagens                        '), nl,
        writeln('            S - Sair do sistema                           '), 
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntradaMenuPrincipal(LowerOption),
        halt. 

processaEntradaMenuPrincipal(Entrada) :- 

        clearScreen,

        ( Entrada == 'c' -> cadastrarUsuario
        ; Entrada == 'd' -> deletarUsuario
        ; Entrada == 'p' -> cadastrarProjeto
        ; Entrada == 'g' -> menuProjetos
        ; Entrada == 'm' -> clearScreen, menuChat
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal ).

cadastrarUsuario :-
        writeln('                                                          '),
        writeln('                  |     Cadastro:    |                    '),
        writeln('                                                          '), nl,
        
        write('Digite seu nome: '),
        ler_string(Nome), nl,
        write('Digite sua senha: '),
        ler_string(Senha), nl,
        random(1000, 9999, IdAtom),
        atom_string(IdAtom, IdUsuario),

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        (nao_vazia(Nome), nao_vazia(Senha) ->
        verifica_id(IdUsuario, UsuariosDoSistema, Existe),
        (Existe ->
                writeln('O usuário já existe. Tente novamente.'), nl, retornoMenuPrincipal
        ;
                salvarUsuario('Database/usuarios.json', Nome, Senha, IdUsuario, []),
                write('Usuário cadastrado com sucesso! O seu ID é: '), writeln(IdUsuario), nl, retornoMenuPrincipal
        )
        ;
         writeln('Nome e senha não podem ser vazios. Tente novamente.'), nl, retornoMenuPrincipal
        ).


deletarUsuario :-
        writeln('                                                          '),
        writeln('               |     Deletar perfil:    |                 '),
        writeln('                                                          '),

        write('Digite seu Id: '),
        ler_string(IdUsuario), nl,
        write('Digite sua senha: '),
        ler_string(Senha), nl,            
        (nao_vazia(IdUsuario), nao_vazia(Senha) ->
                lerJSON('Database/usuarios.json', Usuarios),
                write('Verificando usuário com ID: '), writeln(IdUsuario), nl,
                verifica_id(IdUsuario, Usuarios, Existe),
                        (Existe = true ->
                                verificaSenhaIdUsuario(IdUsuario, Senha, Usuarios) ->
                                     removerUsuario('Database/usuarios.json', IdUsuario)
                                ; 
                                writeln('Senha incorreta. Tente novamente'), nl, retornoMenuPrincipal
                        ;
                        writeln('O usuário não existe. Tente novamente.'), nl, retornoMenuPrincipal
                        )
                ;
                    erroMenuPrincipal
                ).


cadastrarProjeto :-
        writeln('                                                          '),
        writeln('               |     Criar projeto:    |                  '),
        writeln('                                                          '),
        write('Digite seu ID: '),
        ler_string(IdUsuario), nl, 

        write('Digite o nome do projeto: '),
        ler_string(NomeProjeto), nl,
        write('Digite a descrição do projeto: '),
        ler_string(DescricaoProjeto), nl,
        random(1000, 9999, IdAtom),
        atom_string(IdAtom, IdProjeto),
        lerJSON('Database/projetos.json', ProjetosDoSistema), 
        lerJSON('Database/usuarios.json', Usuarios),
                
        (nao_vazia(IdUsuario), nao_vazia(NomeProjeto), nao_vazia(DescricaoProjeto) ->
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, Existe),
        (Existe ->
                writeln('O projeto já existe. Tente novamente.'), nl, retornoMenuPrincipal
        ;
        verifica_id(IdUsuario, Usuarios, ExisteUsuario),
        (ExisteUsuario ->
                salvarProjeto('Database/projetos.json', NomeProjeto, DescricaoProjeto, IdProjeto, [], [], IdUsuario),
                write('Projeto cadastrado com sucesso! O ID do projeto é: '), writeln(IdProjeto), nl, retornoMenuPrincipal       
        ;
        write('Usuario não existe! Tente novamente'), nl, retornoMenuPrincipal
        )
        )                 
        ;
        erroMenuPrincipal
        ).

menuProjetos :-
        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '),
        writeln('                                                          '), nl,
        
        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        (nao_vazia(IdUsuario), ExisteUsuario ->

                lerJSON('Database/projetos.json', ProjetosDoSistema),
                ehGerente(IdUsuario, ProjetosDoSistema, EhGerente),

                (EhGerente -> clearScreen, menuRestritoProjeto
                ; clearScreen, menuPublicoProjeto)

        ; clearScreen,
          writeln('                                                                                                          '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível criar a atividade, tente novamente!  |          '),
          writeln('                                                                                                          ')
        ), retornoMenuPrincipal.

erroMenuPrincipal :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuPrincipal.

%  Exibe erro e retorna ao menuChat
erroMenuChat :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuChat.

menuChat :-
        writeln('                                                                                         '),
        writeln('                            |     Bem-vindo ao Chat!    |                                '), nl,
        writeln('                                                                                         '),
        writeln('    |     Envie mensagens entre membros do seu projeto e usuários do sistema!    |       '), nl,
        writeln('                                                                                         '), 
        writeln('                                  Selecione uma opção:                                   '), nl,
        writeln('                                                                                         '), 
        writeln('                        C - Visualizar mensagens gerais de um projeto                    '), nl,
        writeln('                        H - Visualizar mensagens privadas                                '), nl,
        writeln('                        A - Enviar mensagem geral para membros do projeto                '), nl,
        writeln('                        T - Enviar mensagem privada                                      '), nl,
        writeln('                        M - Voltar ao menu                                               '), nl,
        writeln('                        S - Sair do sistema                                              '), nl,
        writeln('                                                                                         '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntradaMenuChat(LowerOption),
        halt. 

processaEntradaMenuChat(Entrada) :- 

        ( Entrada == 'c' -> clearScreen, visualizarMensagensGerais
        ; Entrada == 'h' -> clearScreen, visualizarMensagensPrivadas
        ; Entrada == 'a' -> clearScreen, enviarMGeral
        ; Entrada == 't' -> clearScreen, enviarMPrivada
        ; Entrada == 'm' -> clearScreen, menuChat
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal ).


        enviarMGeral :-
                writeln('                                                            '),
                writeln('  |  Enviar mensagem para todos os membros do projeto:  |   '),
                writeln('                                                            '),
            
                write('Digite seu ID: '),
                ler_string(IdUsuario),
                nl,
                lerJSON('Database/usuarios.json', UsuariosDoSistema),
                lerJSON('Database/projetos.json', ProjetosDoSistema),
                verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                (ExisteUsuario -> 
                    write('Digite sua senha: '),
                    ler_string(Senha),
                    (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                        writeln('Senha correta'),
                        getUsuarioJSON(IdUsuario,UsuariosDoSistema,Usuario),
                        % Verifica se pertence a algum projeto
                       
                            
                        (ehMembro(IdUsuario, ProjetosDoSistema) -> 
                            writeln(""),
                        
                        writeln('Projetos em que o usuário é gerente: '),
                        imprimirProjetos_Gerente(IdUsuario, ProjetosDoSistema),
                        writeln(""),
                        writeln('Projetos em que o usuário é membro: '),
                        imprimirProjetos_membro(IdUsuario, ProjetosDoSistema),
                        writeln('Escolha o IdProjeto que deseja enviar uma mensagem para seus membros:'),
                            ler_string(IdMensagem),nl,
                            writeln('Digite a mensagem a ser enviada para o IdProjeto selecionado: '),
                            ler_string(Conteudo),nl,
                            %writeln(""),writeln('Carregando....'),sleep(1.5),
                            salvarMensagem('Database/mensagens.json',Usuario.nome,Conteudo,IdMensagem),
                            writeln('                                                            '),
                            writeln('             |  Mensagem enviada com sucesso !  |           '),
                            writeln('                                                            ')
                            ;
                          
                            writeln('                                                            '),
                            writeln('      |  Este usuário não é membro de nenhum projeto!  |    '),
                            writeln('                                                            ')
                        )
                        ;
                        % SE A SENHA INCORRETA: 
                        writeln('                                                            '),
                        writeln('           |  Senha incorreta! Tente novamente!  |          '),
                        writeln('                                                            ')
                    )
                    ;
                    % Usuário não existe
                    writeln('                                                            '),
                    writeln('           |  ID inexistente! Tente novamente!  |           '),
                    writeln('                                                            ')
                
                ), retornoMenuPrincipal.
enviarMPrivada :- 
        writeln('                                                            '),
        writeln('          |  Enviar mensagem para um usuário:  |            '),
        writeln('                                                            '),

        write('Digite seu ID: '),
        % ler_string(IdUsuario),

        % SE O USUÁRIO NÃO EXISTE
        writeln('                                                            '),
        writeln('           |  ID inexistente! Tente novamente!  |           '),
        writeln('                                                            '),

        write('Digite sua senha: '),
        % ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                                                            '),
        writeln('             |  Mensagem enviada com sucesso !  |           '),
        writeln('                                                            '),

        retornoMenuPrincipal.

visualizarMensagensPrivadas :- 
        writeln('                                                            '),
        writeln('           |  Mensagens privadas de um usuário:  |          '),
        writeln('                                                            '),

        write('Digite seu ID: '),
        % ler_string(IdUsuario),

        write('Digite sua senha: '),
        % ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                             '),
        writeln('   Carregando.........       '),

        % VAI COLOCAR O DELAY????
        
        retornoMenuPrincipal.

        visualizarMensagensGerais :-
                writeln('                                                          '),
                writeln('           |  Mensagens gerais de um projeto:  |          '),
                writeln('                                                          '),
        
                write('Digite seu ID: '),
                ler_string(IdUsuario),
                lerJSON('Database/usuarios.json', UsuariosDoSistema),
                lerJSON('Database/projetos.json', ProjetosDoSistema),
                verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                (ExisteUsuario -> 
                    write('Digite sua senha: '),
                    ler_string(Senha),
                    (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                        writeln('Senha correta'),
                        getUsuarioJSON(IdUsuario,UsuariosDoSistema,Usuario),
                        % Verifica se pertence a algum projeto
                       
                            
                        (ehMembro(IdUsuario, ProjetosDoSistema) -> 
                        writeln(""),
                        
                        writeln('Projetos em que o usuário é gerente: '),
                        imprimirProjetos_Gerente(IdUsuario, ProjetosDoSistema),
                        writeln(""),
                        writeln('Projetos em que o usuário é membro: '),
                        imprimirProjetos_membro(IdUsuario, ProjetosDoSistema),
                        writeln('Escolha o IdProjeto que deseja visualizar uma mensagem geral:'),
                            ler_string(IdMensagem),nl,
                            sleep(1.5),
                            writeln('  ________________________________________________________________________________________________________________ '),
        
                            writeln(' | ATENÇÃO : Caixa de Mensagem que nunca receberam nenhuma mensagem de seus membros será representada como vazia  |'),
                            writeln(' |________________________________________________________________________________________________________________|'),
                            writeln(''),
                            writeln(''),
                            writeln('                                                                   '),
                            writeln('                 Carregando.........                               '),
        
                            sleep(1.5),
                            write('Caixa de Mensagem (IdProjeto - '), write(IdMensagem),writeln(')'),
                            exibirMensagens('Database/mensagens.json',IdMensagem)
        
                            ;
                          
                            writeln('                                                            '),
                            writeln('      |  Este usuário não é membro de nenhum projeto!  |    '),
                            writeln('                                                            ')
                        )
                        ;
                        % SE A SENHA INCORRETA: 
                        writeln('                                                            '),
                        writeln('           |  Senha incorreta! Tente novamente!  |          '),
                        writeln('                                                            ')
                    )
                    ;
                    % Usuário não existe
                    writeln('                                                            '),
                    writeln('           |  ID inexistente! Tente novamente!  |           '),
                    writeln('                                                            ')
                
                ), retornoMenuPrincipal.
        
                retornoMenuPrincipal.
% | Retorna ao menu principal ou sai do sistema
retornoMenuPrincipal :- 
        writeln('                                                          '),
        writeln('  | Deseja voltar ao menu principal ou sair do sistema?  |'), nl,
        writeln('                                                          '),
        writeln('                 M - Menu principal                       '), nl,
        writeln('                 S - Sair do sistema                      '),
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),

        (LowerOption == 's' ->
                sairDoSistema, !
                
        ; LowerOption == 'm' ->
                clearScreen,
                menuPrincipal

        ; erroMenuPrincipal).