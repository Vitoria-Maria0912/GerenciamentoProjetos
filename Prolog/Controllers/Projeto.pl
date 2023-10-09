:- module(projeto, [lerProjetosJson/2, projetoToJSON/4, projetosToJSON/2, salvarProjeto/4, exibirProjetosAux/1,
                    exibirProjetos/1,getProjetoJSON/3, removerProjeto/2, removerProjetoJSON/3, verifica_id_projeto/3]).
:- use_module(library(http/json)).

% Lendo arquivo JSON puro
lerProjetosJson(FilePath, File) :-
    open(FilePath, read, F),
    json_read_dict(F, File).

% Cria um usuário
projetoToJSON(NomeProjeto, DescricaoProjeto, IdProjeto, Projeto) :-
    swritef(Projeto, '{"nomeProjeto":"%w", "descricaoProjeto":"%w", "idProjeto":"%w", "atividadesAtribuidas":"%w"}',
[NomeProjeto, DescricaoProjeto, IdProjeto, []]).

% Convertendo uma lista de objetos em JSON para
projetosToJSON([], []).
projetosToJSON([H|T], [U|Projeto]) :-
    projetoToJSON(H.nomeProjeto, H.descricaoProjeto, H.idProjeto, U),
    projetosToJSON(T, Projeto).

% Salvar em arquivo JSON
salvarProjeto(FilePath, NomeProjeto, DescricaoProjeto, IdProjeto) :-
    lerProjetosJson(FilePath, File),
    projetosToJSON(File, ListaProjetos),
    projetoToJSON(NomeProjeto, DescricaoProjeto, IdProjeto, Projetos),
    append(ListaProjetos, [Projetos], Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream).

% Exibe os projetos cadastrados omitindo a descricaoProjeto
exibirProjetosAux([]).
exibirProjetosAux([H|T]) :-
    write('NomeProjeto:'), writeln(H.nomeProjeto),
    write('ID Projeto:'), writeln(H.idProjeto),
		nl, exibirProjetosAux(T).

exibirProjetos(FilePath) :-
		lerProjetosJson(FilePath, Projetos),
		exibirProjetosAux(Projetos).

% Pega uma projeto por ID
getProjetoJSON(IdProjeto, [Projeto|_], Projeto):- IdProjeto == Projeto.idProjeto.
getProjetoJSON(IdProjeto, [_|T], Projeto):- getProjetoJSON(IdProjeto, T, Projeto).

% Removendo um usuário - ainda nao funciona
removerProjetoJSON([], _, []).
removerProjetoJSON([H|T], H.idProjeto, T).
removerProjetoJSON([H|T], Id, [H|Out]):- removerProjetoJSON(T, Id, Out).

removerProjeto(FilePath, Id):-
    lerProjetosJson(FilePath, File),
    removerProjetoJSON(File, Id, SaidaParcial),
    projetosToJSON(SaidaParcial, Saida),
    open(FilePath, write, Stream), write(Stream, Saida), close(Stream),
    writeln('Projeto removido com sucesso. Até a próxima!').

% verifica se um id existe dentro da lista de projetos
verifica_id_projeto(_, [], false).
verifica_id_projeto(Busca, [Projeto|_], true) :-
    get_dict(idProjeto, Projeto, Id),
    Busca == Id.
verifica_id_projeto(Busca, [_|T], R) :- verifica_id_projeto(Busca, T, R).


% falta adicionar atividades a um projeto