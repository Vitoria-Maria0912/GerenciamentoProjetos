:- initialization(menuPrincipal).
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").
:- use_module("Menus/MenuGerente.pl").
:- use_module("Menus/MenuPublico.pl").
:- use_module("Controllers/Utils.pl").

clearScreen :- write("\e[H\e[2J").




% Menu principal com as principais funcionalidades
menuPrincipal :-

        writeln('                                                          '),
        writeln('             |     Menu Principal    |                    '),
        writeln('                                                          '),
        writeln('                 Selecione uma opção:                     '),
        writeln('                                                          '),
        writeln('            C - Cadastrar novo usuário                    '),
        writeln('            D - Deletar perfil                            '),
        writeln('            P - Criar projeto                             '),
        writeln('            G - Menu de projetos                          '),
        writeln('            M - Caixa de mensagens                        '),
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
        ; Entrada == 'a' -> adicionaAtividade
 %       ; Entrada == 'i' -> imprimeUsuario
        ; Entrada == 'm' -> clearScreen, menuChat
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal ).

cadastrarUsuario :-
        writeln('                                                          '),
        writeln('                  |     Cadastro:    |                    '),
        writeln('                                                          '),
        
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
        (nao_vazia(IdUsuario) ->
                lerJSON('Database/usuarios.json', UsuariosDoSistema),


                write('Verificando usuário com ID: '), writeln(IdUsuario), nl,
                verifica_id(IdUsuario, UsuariosDoSistema, Existe),
                        (Existe == true ->
                        removerUsuario('Database/usuarios.json', IdUsuario)
                        ;
                        writeln('O usuário não existe. Tente novamente.'), nl, retornoMenuPrincipal
                        )
                ;
                    erroMenuGeral
                ).

%% PASSAR PRA O MENU GERENTE 
adicionaAtividade :-
        writeln('                                                          '),
        writeln('               |     Alterar nome:    |                 '),
        writeln('                                                          '),
        write('Digite seu Id: '),
        ler_string(IdUsuario), nl,
        write('Digite a nova Atividade: '),
        ler_string(Atividade), nl,

        % FALTA VERIFICAÇÃO SE O ID DA ATIVIDADE EXISTE    
        (nao_vazia(IdUsuario), nao_vazia(Atividade) ->
                lerJSON('Database/usuarios.json', UsuariosDoSistema),
                verifica_id(IdUsuario, UsuariosDoSistema, Existe),
                        (Existe == true ->
                        editarAtividades('Database/usuarios.json', IdUsuario, Atividade),
                        write('Sucesso!')
                        ;
                        writeln('O usuário não existe. Tente novamente.'), nl, retornoMenuPrincipal
                        )
                ;
                    erroMenuGeral
                ).

% teste pra ver se getUsuario está funcionando
% imprimeUsuario :-
%         write('Digite seu Id: '),
%         ler_string(IdUsuario), nl,
%         (nao_vazia(IdUsuario) ->
%                 lerJSON('Database/usuarios.json', UsuariosDoSistema),
%                 getUsuarioJSON(IdUsuario, UsuariosDoSistema, Usuario),
%                 write(Usuario);
%                 erroMenuGeral).


cadastrarProjeto :-
        writeln('                                                          '),
        writeln('               |     Criar projeto:    |                  '),
        writeln('                                                          '),
                
        %Falta adicionar o id de usuário para que só gerentes criem
        write('Digite o nome do projeto: '),
        ler_string(NomeProjeto), nl,
        write('Digite a descrição do projeto: '),
        ler_string(DescricaoProjeto), nl,
        random(1000, 9999, IdAtom),
        atom_string(IdAtom, IdProjeto),
        lerProjetosJson('Database/projetos.json', ProjetosDoSistema),
                
        (nao_vazia(NomeProjeto), nao_vazia(DescricaoProjeto) ->
        verifica_id_projeto(IdProjeto, ProjetosDoSistema, Existe),
        (Existe ->
                        writeln('O projeto já existe. Tente novamente.'), nl, retornoMenuPrincipal
                        ;
                        salvarProjeto('Database/projetos.json', NomeProjeto, DescricaoProjeto, IdProjeto, [], []),
                        write('Projeto cadastrado com sucesso! O ID do projeto é: '), writeln(IdProjeto), nl, retornoMenuPrincipal
                        )
                        ;
                                writeln('Nome e descrição do projeto não podem ser vazios. Tente novamente.'), nl, retornoMenuPrincipal
                        ),
                        retornoMenuPrincipal.

menuProjetos :-
        clearScreen,
        
        % FAZER A VERIFICAÇÃO DO EHGERENTE

        menuRestritoProjeto.
        % menuPublicoProjeto.
        
        % halt. 


menuChat :- 
        writeln('                                                                        '),
        writeln('                    |      Bem-vindo ao Chat!    |                      '),
        writeln('                                                                        '),
        writeln('  Envie mensagens entre membros do seu projeto e usuários do sistema!   '),
        writeln('                                                                        '),
        writeln('                          Selecione uma opção:                          '),
        writeln('                                                                        '),
        writeln('            C - Visualizar mensagens gerais de um projeto               '),
        writeln('            H - Visualizar mensagens privadas                           '),
        writeln('            A - Enviar mensagem geral para membros do projeto           '),
        writeln('            T - Enviar mensagem privada                                 '),
        writeln('            M - Voltar ao menu principal                                '),
        writeln('            S - Sair do sistema                                         '),
        writeln('                                                                        '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntradaChat(LowerOption),
        halt. 

processaEntradaChat(Entrada) :- 

        clearScreen,

        ( Entrada == 'c' -> enviarMPrivada
        ; Entrada == 'h' -> enviarMGeral
        ; Entrada == 'a' -> visualizarMensagensPrivadas
        ; Entrada == 't' -> visualizarMensagensGerais
        ; Entrada == 'm' -> menuPrincipal
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal ).

enviarMPrivada :-
        clearScreen, 
        writeln('                                                                    '),
        writeln('                |     Enviar mensagem para um usuário:    |         '),
        writeln('                                                                    '),
        menuChat.

enviarMGeral :-
        clearScreen,
        writeln('                                                                    '),
        writeln('      |     Enviar mensagem para todos os membros do projeto:    |  '),
        writeln('                                                                    '),
        menuChat.   

visualizarMensagensPrivadas :-
        clearScreen,
        writeln('                                                                    '),
        writeln('                |     Mensagens privadas de um usuário:    |        '),
        writeln('                                                                    '),
        menuChat.

visualizarMensagensGerais :-
        clearScreen,
        writeln('                                                                 '),
        writeln('              |     Mensagens gerais de um projeto:    |         '),
        writeln('                                                                 '),
        menuChat.

erroMenuPrincipal :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuPrincipal.

sairDoSistema :-
        clearScreen,
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '), !.

%  Exibe erro e retorna ao menuChat
erroMenuChat :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        menuChat.

menuChat :-
        writeln('                                                                                         '),
        writeln('                            |     Bem-vindo ao Chat!    |                                '),
        writeln('                                                                                         '),
        writeln('    |     Envie mensagens entre membros do seu projeto e usuários do sistema!    |       '),
        writeln('                                                                                         '),
        writeln('                                  Selecione uma opção:                                   '),
        writeln('                                                                                         '),
        writeln('                        C - Visualizar mensagens gerais de um projeto                    '),
        writeln('                        H - Visualizar mensagens privadas                                '),
        writeln('                        A - Enviar mensagem geral para membros do projeto                '),
        writeln('                        T - Enviar mensagem privada                                      '),
        writeln('                        M - Voltar ao menu                                                                 '),
        writeln('                        S - Sair do sistema                                              '),
        writeln('                                                                                         '),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption),
        processaEntradaMenuChat(LowerOption),
        halt. 

processaEntradaMenuChat(Entrada) :- 

        clearScreen,

        ( Entrada == 'c' -> visualizarMensagensGerais
        ; Entrada == 'h' -> visualizarMensagensPrivadas
        ; Entrada == 'a' -> enviarMGeral
        ; Entrada == 't' -> enviarMPrivada
        ; Entrada == 'm' -> clearScreen, menuChat
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuPrincipal ).

visualizarMensagensGerais :-
        writeln('                                                          '),
        writeln('           |  Mensagens gerais de um projeto:  |          '),
        writeln('                                                          '),

        write('Digite seu ID: '),
        ler_string(IdUsuario),

        % SE O USUÁRIO NÃO EXISTE
        writeln('                                                            '),
        writeln('           |  ID inexistente! Tente novamente!  |           '),
        writeln('                                                            '),

        write('Digite sua senha: '),
        ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                                                                '),
        write('Digite o ID do Projeto que deseja visualizar as mensagens gerais: '),
        ler_string(IdProjeto),

        writeln('                                                                   '),
        writeln('   Carregando.........                                             '),

        % VAI COLOCAR O DELAY????

        % \+ usuarioEstaEmAlgumProjeto , AINDA TEM QUE FAZER
        writeln('                                                                  '),
        writeln('           |  Usuário não é membro de nenhum projeto.  |          '),
        writeln('                                                                  '),

        retornoMenuPrincipal.

visualizarMensagensPrivadas :- 
        writeln('                                                            '),
        writeln('           |  Mensagens privadas de um usuário:  |          '),
        writeln('                                                            '),

        write('Digite seu ID: '),
        ler_string(IdUsuario),

        write('Digite sua senha: '),
        ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                             '),
        writeln('   Carregando.........       '),

        % VAI COLOCAR O DELAY????
        
        retornoMenuPrincipal.

enviarMGeral :- 
        writeln('                                                            '),
        writeln('  |  Enviar mensagem para todos os membros do projeto:  |   '),
        writeln('                                                            '),

        write('Digite seu ID: '),
        ler_string(IdUsuario),

        % SE O USUÁRIO NÃO EXISTE
        writeln('                                                            '),
        writeln('           |  ID inexistente! Tente novamente!  |           '),
        writeln('                                                            '),

        write('Digite sua senha: '),
        ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                                                            '),
        writeln('             |  Mensagem enviada com sucesso !  |           '),
        writeln('                                                            '),

        writeln('                                                            '),
        writeln('      |  Este usuário não é membro de nenhum projeto!  |    '),
        writeln('                                                            '),

        retornoMenuPrincipal.

enviarMPrivada :- 
        writeln('                                                            '),
        writeln('          |  Enviar mensagem para um usuário:  |            '),
        writeln('                                                            '),

        write('Digite seu ID: '),
        ler_string(IdUsuario),

        % SE O USUÁRIO NÃO EXISTE
        writeln('                                                            '),
        writeln('           |  ID inexistente! Tente novamente!  |           '),
        writeln('                                                            '),

        write('Digite sua senha: '),
        ler_string(Senha),

        % SE A SENHA INCORRETA: 
        writeln('                                                            '),
        writeln('           |  Senha incorreta! Tente novamente!  |          '),
        writeln('                                                            '),

        % FALTA MUITA COISA, olhar no de haskell

        writeln('                                                            '),
        writeln('             |  Mensagem enviada com sucesso !  |           '),
        writeln('                                                            '),

        retornoMenuPrincipal.

% | Retorna ao menu principal ou sai do sistema
retornoMenuPrincipal :- 
        writeln('                                                          '),
        writeln('  | Deseja voltar ao menu principal ou sair do sistema?  |'),
        writeln('                                                          '),
        writeln('                 M - Menu principal                       '),
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


% Jamilly precisará para o Chat

/**
 * waitInput is det.
 * 
 * Espera o usuário digitar algum caractere.
 */ 
% waitInput:-waitInput("").

/**
 * waitInput(+S:string) is det. 
 * 
 * Imprime a mensagem fornecida, esperando o usuário digitar algum caractere.
 * @param S Mensagem a ser impressa antes de esperar a entrada
 */
% waitInput(S):-
%     ansi_format([bold,fg(yellow)], "~w", [S]),
%     ansi_format([bold,fg(yellow)], "~w", ["Aperte qualquer tecla para continuar."]),
%     get_single_char(_),nl.
