

:- module(menuGerente, [menuRestritoProjeto/0, processaEntradaMenuRestrito/1, visualizarProjetos/0, deletarProjeto/0, gerenciarMembros/0, processaEntradaMembros/0]).

:- initialization(menuRestritoProjeto).
:- use_module("Controllers/Atividades.pl").

clearScreen :- write("\e[H\e[2J"). % só serve no unix

% | Menu dos projetos, apenas os gerentes têm acesso
menuRestritoProjeto :-

        writeln(''),
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

        clearScreen,

        ( Entrada == 'l' -> visualizarProjetos
        ; Entrada == 'p' -> deletarProjeto
        ; Entrada == 'g' -> gerenciarMembros
        ; Entrada == 'b' -> menuBancoDeAtividades
        ; Entrada == 'i' -> comecarAtividade
        ; Entrada == 'i' -> comecarAtividade
        ; Entrada == 'v' -> visualizarAtividades
        ; Entrada == 'a' -> visualizarStatusAtividade
        ; Entrada == 'o' -> criaFeedback
        ; Entrada == 'm' -> consult('Menus/MenuGeral.pl')
        ; Entrada == 's' -> sairDoSistema
        ; erroMenuGerente ).

visualizarProjetos :-
        writeln('                                                          '),
        writeln('         |  Estes são os projetos no sistema:  |          '),
        writeln('                                                          '),
        retornoMenuRestrito. 
%         visualizarProjetos.

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
        ).
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
        ; LowerOption == 'v' -> consult('Menus/MenuGeral.pl')
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
        random(10000, 99999, Idatom),
        atom_string(Idatom,IdAtividade),
        % Falta colocar verificação de entrada
        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),

        (\+ atividadeJaExiste(IdAtividade, AtividadesDoSistema) -> 
                salvarAtividade('Database/bancoDeAtividades.json', Titulo, Descricao, Dificuldade, IdAtividade), 
                write('Atividade criada! E o ID dela é: '), writeln(IdAtividade), nl
        ; erroMenuGerente),

        writeln('Deseja adicionar a atividade a um projeto? (S/N)'),
        get_single_char(CodigoASCII),
        char_code(Input, CodigoASCII), 
        downcase_atom(Input, LowerOption), nl,

        ( LowerOption == 's' -> write('Digite o ID do projeto que deseja adicionar a atividade: '),
                                read_string(user_input, "\n", "\r", _, IdProjetoAtividade), nl,

                                % É NECESSÁRIA A VERIFICAÇÃO DO PROJETO!!!         <<<<< Não está alterando >>>>>>>>

                                editarIdProjetoAtividadeJSON(AtividadesDoSistema, IdAtividade, IdProjetoAtividade, Out),
                                writeln('Atividade alterada com sucesso!'),
                                retornoMenuRestrito
        ; LowerOption == 'n' -> retornoMenuRestrito
        ; erroMenuGerente).

% Deleta uma atividade do projeto(Inicialmente remove pelo o ID da atividade)    <<<< Falta fazer verificação se é vazio as entradas>>>>>>>>>>>>>>>>>>>
deletaAtividade :-
        writeln('                                                       '),
        writeln('               |  Deletar  atividade:  |               '),
        writeln('                                                       '),
        
        %write('Digite o ID da Atividade a ser deletada: '),
        %%ler_string(Id_Atividade), nl,
        %---- DEVERIA SER ESSA CONFIGURAÇÃO , COM BASE NO ID DO PROJETO PASSADO
        write('Digite o ID do projeto que terá a atividade a ser deletada: '),
        ler_string(IdProjeto), nl,
        % Sem a leitura abaixo não consigo remover
        (nao_vazia(IdProjeto) ->
        write('urunu'),
                        lerProjetosJson('Database/projetos.json', ProjetosDoSistema),
                        lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),
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
                                ;                     
                                erroMenuGerente
                        ).
        
        


       lerBancoDeAtividadesJson('Database/bancoDeAtividades.json', AtividadesDoSistema),
       removerAtividade('Database/bancoDeAtividades.json', X).
  
        retornoMenuRestrito.

listarAtividades :-
        writeln('                                                       '),
        writeln('          |  Listar Atividades cadastradas      :  |   '),
        writeln('                                                       '),

        exibirAtividades('Database/bancoDeAtividades.json').
sairDoSistema :-
        clearScreen,
        writeln('                                                          '),
        writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
        writeln('                                                          '),
        halt. 

erroMenuGerente :-
        clearScreen,
        writeln('                                                          '),
        writeln('         |  Entrada Inválida. Tente novamente!  |         '),
        writeln('                                                          '),
        retornoMenuRestrito.

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

        (LowerOption == 's' -> sairDoSistema, !
                
        ; LowerOption == 'p' -> menuRestritoProjeto, !

        ; LowerOption == 'm' -> consult('Menus/MenuGeral.pl'), !
        
        ; erroMenuGerente ).
