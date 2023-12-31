:- module(menuGeral, [menuPrincipal/0, cadastrarUsuario/0, deletarUsuario/0, cadastrarProjeto/0,
                      menuProjetos/0, menuChat/0, enviarMPrivada/0, enviarMGeral/0, visualizarMensagensPrivadas/0,
                      visualizarMensagensGerais/0, erroMenuPrincipal/0, erroMenuChat/0, menuChat/0,
                      retornoMenuPrincipal/0]).

:- initialization(menuPrincipal).

:- use_module("Menus/MenuGerente.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").

:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Controllers/Mensagem.pl").


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

        (LowerOption == 's' -> sairDoSistema, !
                
        ; LowerOption == 'm' -> clearScreen, menuPrincipal

        ; erroMenuPrincipal).

% | Exibe erro e retorna ao menuPrincipal
erroMenuPrincipal :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuPrincipal.

% | Exibe erro e retorna ao menuChat
erroMenuChat :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuChat.


% Menu principal com as principais funcionalidades
menuPrincipal :-

        writeln('                                                          '),
        writeln('             |     Menu Principal    |                    '), nl,
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '), nl,
        writeln('                                                          '),
        writeln('            C - Cadastrar usuário                         '), nl,
        writeln('            D - Deletar perfil                            '), nl,
        writeln('            P - Criar projeto                             '), nl,
        writeln('            G - Menu de projetos                          '), nl,
        writeln('            M - Caixa de mensagens                        '), nl,
        writeln('            S - Sair do sistema                           '), 
        writeln('                                                          '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, Entrada),

        ( Entrada == 'c' -> clearScreen, cadastrarUsuario
        ; Entrada == 'd' -> clearScreen, deletarUsuario
        ; Entrada == 'p' -> clearScreen, cadastrarProjeto
        ; Entrada == 'g' -> clearScreen, menuProjetos
        ; Entrada == 'm' -> clearScreen, menuChat
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal), 
        
        retornoMenuPrincipal.

% | Cadastra um usuário no sistema
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
                        writeln('       |    O usuário já existe. Tente novamente.   |'), nl
                ;
                        salvarUsuario('Database/usuarios.json', Nome, Senha, IdUsuario, []), nl,
                        write(' |    Usuário cadastrado com sucesso! O seu ID é: '), write(IdUsuario), writeln('   |'), nl
                )
        ;
         writeln('      |    Nome e senha não podem ser vazios. Tente novamente.    |'), nl
        ).

% | Deleta um usuário do sistema
deletarUsuario :-
        writeln('                                                          '),
        writeln('               |     Deletar perfil:    |                 '),
        writeln('                                                          '), nl,

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,         

        lerJSON('Database/usuarios.json', Usuarios),
       
        verifica_id(IdUsuario, Usuarios, Existe),

        (Existe ->
                write('Digite sua senha: '),
                ler_string(Senha), nl, nl,
                write('     |       Verificando usuário com ID: '), write(IdUsuario), writeln(' ............'), nl,
                sleep(1.5),
                (verificaSenhaIdUsuario(IdUsuario, Senha, Usuarios) ->
                        removerUsuario('Database/usuarios.json', IdUsuario)
                ; 
                nl, writeln('       |      Senha incorreta. Tente novamente.        |'), nl
                )        

        ; clearScreen,
        writeln('                                                                                  '),
        writeln(' |  Usuário inexistente, não foi possível deletá-lo, tente novamente!  |          '),
        writeln('                                                                                  ')
        ).

% | Cadastra um projeto no sistema
cadastrarProjeto :-
        writeln('                                                          '),
        writeln('               |     Criar projeto:    |                  '),
        writeln('                                                          '), nl,
        write('Digite seu ID: '),
        ler_string(IdUsuario), nl, 

        lerJSON('Database/projetos.json', ProjetosDoSistema), 
        lerJSON('Database/usuarios.json', Usuarios),

        verifica_id(IdUsuario, Usuarios, ExisteUsuario),

        (ExisteUsuario ->

                write('Digite o nome do projeto: '),
                ler_string(NomeProjeto), nl,
                write('Digite a descrição do projeto: '),
                ler_string(DescricaoProjeto), nl,

                random(1000, 9999, IdAtom),
                atom_string(IdAtom, IdProjeto),

                verifica_id_projeto(IdProjeto, ProjetosDoSistema, Existe),

                (nao_vazia(NomeProjeto), nao_vazia(DescricaoProjeto) ->

                        (Existe -> nl, writeln('|      O projeto já existe. Tente novamente.'), nl
                        
                        ; salvarProjeto('Database/projetos.json', NomeProjeto, DescricaoProjeto, IdProjeto, [], [], IdUsuario),
                        nl, write('    |    Projeto cadastrado com sucesso! O ID do projeto é: '), write(IdProjeto), writeln('   |') , nl, nl,

                        writeln('Deseja adicionar um membro ao projeto? (S/N)'),
                        get_single_char(CodigoASCII),
                        char_code(Input, CodigoASCII), 
                        downcase_atom(Input, LowerOption), nl,

                        ( LowerOption == 's' -> adicionaNovoMembro(IdProjeto)
                
                        ; LowerOption == 'n' -> menuPrincipal
                        ; erroMenuProjeto)
                        )

                ; clearScreen,
                writeln('                                                                                                          '),
                writeln(' |  Campo obrigatório vazio, não foi possível cadastrar o projeto, tente novamente!  |          '),
                writeln('                                                                                                          ')
                )
                        
                

        ; nl, write('       |    Usuario não existe! Tente novamente.    |'), nl, nl
        ).
                

% | Exibe o menu de acordo com o idUsuario, se gerente (pois já criou um projeto) o restrito, caso contrário, o público 
menuProjetos :-
        writeln('                                                          '),
        writeln('             |     Menu de projetos:    |                 '),
        writeln('                                                          '), nl,
        
        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        (ExisteUsuario ->

                lerJSON('Database/projetos.json', ProjetosDoSistema),
                ehGerente(IdUsuario, ProjetosDoSistema, EhGerente),

                (EhGerente -> clearScreen, menuRestritoProjeto
                ; clearScreen, menuPublicoProjeto)

        ; clearScreen,
          writeln('                                                                                                          '),
          writeln(' |  Campo obrigatório vazio ou inválido, não foi possível entrar no menu de projetos, tente novamente!  |          '),
          writeln('                                                                                                          ')
        ).


% | Exibe o menu da caixa de mensagens
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
        writeln('                        M - Voltar ao menu principal                                     '), nl,
        writeln('                        S - Sair do sistema                                              '), nl,
        writeln('                                                                                         '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, Entrada),

        ( Entrada == 'c' -> clearScreen, visualizarMensagensGerais
        ; Entrada == 'h' -> clearScreen, visualizarMensagensPrivadas
        ; Entrada == 'a' -> clearScreen, enviarMGeral
        ; Entrada == 't' -> clearScreen, enviarMPrivada
        ; Entrada == 'm' -> clearScreen, menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal),
        
        retornoMenuPrincipal.


% | Exibe as mensagem de um projeto específico
visualizarMensagensGerais :-
        writeln('                                                          '),
        writeln('           |  Mensagens gerais de um projeto:  |          '), nl,
        writeln('                                                          '),

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,

        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        lerJSON('Database/projetos.json', ProjetosDoSistema),
        lerJSON('Database/mensagens.json', MensagensDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),

        (ExisteUsuario -> 
            write('Digite sua senha: '),
            ler_string(Senha), nl,

            (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
               
                (ehMembro(IdUsuario, ProjetosDoSistema) -> 
                writeln(""),
                
                writeln('   |    Projetos em que o usuário é gerente:   |'), nl,
                imprimirProjetos_Gerente(IdUsuario, ProjetosDoSistema), nl,
                writeln('--------------------------------------------------'), nl,
                writeln('    |    Projetos em que o usuário é membro:    |'), nl,
                imprimirProjetos_membro(IdUsuario, ProjetosDoSistema), nl, nl,

                write('Digite o ID do projeto que deseja visualizar as mensagens gerais: '),
                    ler_string(IdMensagem),
                    getProjetoJSON(IdMensagem,ProjetosDoSistema,Projeto),
                    (membroDoProjeto(IdUsuario,Projeto) ->
                    sleep(1.5),
                    writeln(''),
                    writeln('                                                                   '),
                    writeln('                 Carregando.........                               '),
                    writeln(''),
                    writeln(''),
                    sleep(1.5),

                    verifica_id_mensagem(IdMensagem,MensagensDoSistema,ExisteMensagem),
                    (ExisteMensagem -> 
                        write('Caixa de Mensagem (IdProjeto - '), write(IdMensagem),writeln(')'),
                        exibirMensagens('Database/mensagens.json',IdMensagem),
                        sleep(1.5), nl
                ;

                writeln('  ________________________________________________________ '),
                writeln(' |                                                        |'),
                writeln(' |                Caixa de mensagens vazia!               |'),
                writeln(' |________________________________________________________|'), nl
                )
                    ;

                    writeln('                                                            '),
                    writeln('      |  Este usuário não é membro desse projeto    !  |    '),
                    writeln('                                                            ')
                    )
                  ;
                    writeln('                                                            '),
                    writeln('      |  Este usuário não é membro de nenhum projeto!  |    '),
                    writeln('                                                            ')
                )
                ;
                writeln('                                                            '),
                writeln('           |  Senha incorreta! Tente novamente!  |          '),
                writeln('                                                            ')
            )
            ;
            % Usuário não existe
            writeln('                                                            '),
            writeln('           |  ID inexistente! Tente novamente!  |           '),
            writeln('                                                            ')
        ). 

% | Exibe as mensagens de um usuário específico
visualizarMensagensPrivadas:- 
        writeln('                                                            '),
        writeln('           |  Mensagens privadas de um usuário:  |          '), nl,
        writeln('                                                            '),

        write('Digite seu ID: '),
        ler_string(IdUsuario),nl,
        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        lerJSON('Database/mensagens.json', MensagensDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
        (ExisteUsuario -> 
        write('Digite sua senha: '),
        ler_string(Senha),
        (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                sleep(1.5),
                writeln(''),
                writeln('                                                                   '),
                writeln('                 Carregando.........                               '),
                writeln(''),
                writeln(''),
                sleep(1.5),
                verifica_id_mensagem(IdUsuario,MensagensDoSistema,ExisteMensagem),
                (ExisteMensagem -> 
                        write('Caixa de Mensagem (IdUsuario - '), write(IdUsuario),writeln(')'),
                        exibirMensagens('Database/mensagens.json',IdUsuario),
                        sleep(1.5), nl
                ;
                writeln('  ________________________________________________________ '),
                writeln(' |                                                        |'),
                writeln(' |                Caixa de mensagens vazia!               |'),
                writeln(' |________________________________________________________|'),
                sleep(1.5), nl
                )
                ;
                
                writeln('                                                            '),
                writeln('           |  Senha incorreta! Tente novamente!  |          '),
                writeln('                                                            ')
        ) ;
        writeln('                                                            '),
        writeln('           |  ID inexistente! Tente novamente!  |           '),
        writeln('                                                            ')

        ).

% | Envia uma mensagem para todos os membros de um projeto
enviarMGeral :-
                writeln('                                                            '),
                writeln('  |  Enviar mensagem para todos os membros do projeto:  |   '), nl,
                writeln('                                                            '),
            
                write('Digite seu ID: '),
                ler_string(IdUsuario), nl,

                lerJSON('Database/usuarios.json', UsuariosDoSistema),
                lerJSON('Database/projetos.json', ProjetosDoSistema),
                verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                (ExisteUsuario -> 
                    write('Digite sua senha: '),
                    ler_string(Senha),
                    (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                        writeln(''),
                        getUsuarioJSON(IdUsuario,UsuariosDoSistema,Usuario),
                            
                        (ehMembro(IdUsuario, ProjetosDoSistema) -> 
                        writeln(""),
                        
                        writeln('   |    Projetos em que o usuário é gerente:     |'), nl,
                        imprimirProjetos_Gerente(IdUsuario, ProjetosDoSistema), nl,
                        writeln('--------------------------------------------------'), nl,
                        writeln('   |   Projetos em que o usuário é membro:    |   '), nl,
                        imprimirProjetos_membro(IdUsuario, ProjetosDoSistema),writeln(''), nl,

                        write('Digite o ID do projeto que deseja enviar uma mensagem para seus membros: '),
                            ler_string(IdMensagem),nl,
                            getProjetoJSON(IdMensagem,ProjetosDoSistema,Projeto),
                            (membroDoProjeto(IdUsuario,Projeto) ->
                            write('Digite a mensagem a ser enviada para o IdProjeto selecionado: '),
                            ler_string(Conteudo),nl,
                            (nao_vazia(Conteudo) ->
                            salvarMensagem('Database/mensagens.json',Usuario.nome,Conteudo,IdMensagem),
                            writeln('                                                            '),
                            writeln('             |  Mensagem enviada com sucesso!  |            '),
                            writeln('                                                            ')

                            ; nl,
                            writeln('      |    Mensagem não pode ser vazia. Tente novamente.    |'), nl
                            )

                            ;

                            writeln('                                                            '),
                            writeln('      |  Este usuário não é membro desse projeto!  |        '),
                            writeln('                                                            ')
                            )
                            ;
                          
                            writeln('                                                            '),
                            writeln('      |  Este usuário não é membro de nenhum projeto!  |    '),
                            writeln('                                                            ')
                        )
                        ;
                        writeln('                                                            '),
                        writeln('           |  Senha incorreta! Tente novamente!  |          '),
                        writeln('                                                            ')
                    )
                    ;
                    % Usuário não existe
                    writeln('                                                            '),
                    writeln('           |  ID inexistente! Tente novamente!  |           '),
                    writeln('                                                            ')
                
                ).

% | Envia uma mensagem para um usuário específico
enviarMPrivada :- 
        writeln('                                                            '),
        writeln('          |  Enviar mensagem para um usuário:  |            '), nl,
        writeln('                                                            '),

        write('Digite seu ID: '),
        ler_string(IdUsuario), nl,
        lerJSON('Database/usuarios.json', UsuariosDoSistema),
        verifica_id(IdUsuario, UsuariosDoSistema, ExisteUsuario),
                (ExisteUsuario -> 
                    write('Digite sua senha: '),
                    ler_string(Senha), nl,

                    (verificaSenhaIdUsuario(IdUsuario, Senha, UsuariosDoSistema) ->
                        writeln(''),
                        getUsuarioJSON(IdUsuario,UsuariosDoSistema,Usuario),

                        writeln('        |   Estes são os usuários do sistema:    |'), nl, nl,
                        exibeUsuarios_id_nome('Database/usuarios.json'), nl,
                        % Pede o ID que terá a mensagem privada encaminhada
                        write('Digite o ID usuário para quem deseja enviar uma mensagem privada: '),
                        ler_string(IdDestinatario), nl,
                        (IdUsuario == IdDestinatario ->
                        writeln(''),
                        writeln('         |    Remetente e destinatário da mensagem não podem ser iguais. Tente novamente.     |'), nl
                        ;
                        verifica_id(IdDestinatario, UsuariosDoSistema, ExisteDestinatario),    
                        (ExisteDestinatario -> 
                        write('Digite a mensagem a ser enviada para o usuário escolhido: '),
                        ler_string(ConteudoMsg),nl,
                        (nao_vazia(ConteudoMsg)->
                            salvarMensagem('Database/mensagens.json',Usuario.nome,ConteudoMsg,IdDestinatario),
                            writeln('                                                            '),
                            writeln('             |  Mensagem enviada com sucesso !  |           '),
                            writeln('                                                            ')
                        ;
                        writeln('      |    Mensagem não pode ser vazia. Tente novamente.    |'), nl
                        )
                )
                            ;
                          
                            writeln('                                                         '),
                            writeln('    |  Este ID destinatário não existe no sistema!  |    '),
                            writeln('                                                         ')
                        )
                        ;
                        writeln('                                                            '),
                        writeln('           |  Senha incorreta! Tente novamente!  |          '),
                        writeln('                                                            ')
                    )
                    ;
                    % Usuário não existe
                    writeln('                                                            '),
                    writeln('           |  ID inexistente! Tente novamente!  |           '),
                    writeln('                                                            ')
                
                ).
