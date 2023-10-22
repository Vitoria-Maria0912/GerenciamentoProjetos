:- module( mensagem, [lerJSON/2, mensagemToJSON/5, mensagensToJSON/2, salvarMensagem/4, exibirMensagensAux/1,exibirMensagens/1]).

:- use_module(library(http/json)).
:- use_module("Controllers/Utils.pl").


% Cria uma mensagem
mensagemToJSON(NomeDestinatario, ConteudoMensagem, IdMensagem, Mensagem) :-
    swritef(Mensagem, '{"nomeDestinatario":"%w", "conteudoMensagem":"%w", "idMensagem":"%w"}',
    [NomeDestinatario, ConteudoMensagem, IdMensagem]).

% Convertendo uma lista de objetos em JSON
mensagensToJSON([], []).
mensagensToJSON([H|T], [Mensagem|Mensagens]) :-
    mensagemToJSON(H.nomeDestinatario, H.conteudoMensagem, H.idMensagem, Mensagem),
    mensagensToJSON(T, Mensagens).

% Salvar em arquivo JSON
salvarMensagem(FilePath, NomeDestinatario, ConteudoMensagem, IdMensagem) :-
    lerJSON(FilePath, File),
    mensagensToJSON(File, ListaMensagens),
    mensagemToJSON(NomeDestinatario, ConteudoMensagem, IdMensagem, Mensagem),
    append(ListaMensagens, [Mensagem], Saida),
    open(FilePath, write, Stream),
    write(Stream, Saida),
    close(Stream).

% Exibe os mensagens cadastrados 
%falta checar idMensagem
exibirMensagensAux([]).
exibirMensagensAux([H|T]) :-
    
    write('Mensagem enviada por: '), writeln(H.nomeDestinatario),
    write('ID Projeto: '), writeln(H.idMensagem),
		nl,
    write(H.conteudoMensagem), exibirMensagensAux(T).

exibirMensagens(FilePath) :-
		lerJSON(FilePath, Mensagens),
		exibirMensagensAux(Mensagens).
