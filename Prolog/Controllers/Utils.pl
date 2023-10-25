:- module(utils, [nao_vazia/1, ler_string/1, clearScreen/0, lerJSON/2, verificaSenhaIdUsuario/3, gerenteDoProjeto/3, string_para_numero/2, ids_sao_iguais/2, sairDoSistema/0]).

:- use_module(library(http/json)).
:- use_module("Controllers/Usuario.pl").
:- use_module("Controllers/Projeto.pl").


% | Verifica se a entrada é vazia
nao_vazia(Input) :-
    Input \= "".

ids_sao_iguais(ID1, ID2) :-
    Id1 == Id2.


% | Lê uma entrada do usuário
ler_string(X) :-
    read_line_to_codes(user_input, R),
    atom_string(R, X).

% | Limpa a tela, apagando os comandos anteriores
clearScreen :- shell('clear'). % serve para unix 
clearScreen :- shell('cls'). % seve para windows

% Lê um arquivo JSON
lerJSON(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).

% | Verifica se a senha pertence ao usuário
verificaSenhaIdUsuario(IdUsuario, Senha, Usuarios) :-
    getUsuarioJSON(IdUsuario, Usuarios, Usuario),
    Usuario.senha == Senha.

% | Verifica se o usuário é gerente do projeto
gerenteDoProjeto(IdProjeto, IdUsuario, Projetos) :-
    getProjetoJSON(IdProjeto, Projetos, Projeto),
    Projeto.idGerente == IdUsuario.

% | Transforma uma string em um número
string_para_numero(String, Numero) :-
    number_string(Numero, String).

% | Sai do sistema
sairDoSistema :-
    clearScreen,
    writeln('                                                          '),
    writeln('        |  Você saiu do sistema! Até a próxima!  |        '),
    writeln('                                                          '), 
    halt.

