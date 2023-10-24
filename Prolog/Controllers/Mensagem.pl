:- module( mensagem, [mensagemToJSON/4, mensagensToJSON/2, salvarMensagem/4, exibirMensagensAux/2,verifica_id_mensagem/3, exibirMensagens/2]).

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

% verifica se um id existe dentro da lista de mensagens
verifica_id_mensagem(_, [], false).
verifica_id_mensagem(Busca, [Mensagem|_], true) :- 
    get_dict(idMensagem, Mensagem, Id),
    Busca == Id.
verifica_id_mensagem(Busca, [_|T], R) :- verifica_id_mensagem(Busca, T, R).



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
exibirMensagensAux([],_).
exibirMensagensAux([H|T],IdCaixa) :-
    (IdCaixa == H.idMensagem ->
    writeln('__________________________________________________________________'),
    write('Mensagem enviada por: '), write(H.nomeDestinatario), writeln('  ✔ '),
    write('ID Projeto: '), write(H.idMensagem),nl,write('"'),
    write(H.conteudoMensagem),writeln('"'), exibirMensagensAux(T,IdCaixa);exibirMensagensAux(T,IdCaixa)).

exibirMensagens(FilePath,IdCaixa) :-
		lerJSON(FilePath, Mensagens),
		exibirMensagensAux(Mensagens,IdCaixa).
