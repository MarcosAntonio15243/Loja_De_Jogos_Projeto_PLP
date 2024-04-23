:- module(jogo,
    [getJogos/2,
    getJogosById/3,
    getJogosByNome/3,
    getJogosUntilOnePrice/3,
    getJogosMinimumPrice/3,
    getJogosByGender/3,
    getJogosOrderByDate/2,
    getJogosOrderByName/2,
    getJogosOrderByPrice/2,
    getJogosOrderByBiggestPrice/2,
    getJogosOrderByRating/2,
    print_jogos/1,
    print_jogo/1,
    print_jogo_detalhado/1]).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").


getJogos(Connection, Jogos):-
    Q = "SELECT * FROM jogo",
    db_query(Connection, Q, Jogos).

getJogosById(Connection, JogoId, Jogo):-
    Q = "SELECT * FROM jogo WHERE game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], Jogo).

getJogosByNome(Connection, Nome, Jogo):-
    Q = "SELECT * FROM jogo WHERE LOWER(game_nome) = LOWER('%w')",
    db_parameterized_query(Connection, Q, [Nome], Jogo).

getJogosUntilOnePrice(Connection, Preco, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_price <= %w",
    db_parameterized_query(Connection, Q, [Preco], Jogos).

getJogosMinimumPrice(Connection, Preco, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_price >= %w",
    db_parameterized_query(Connection, Q, [Preco], Jogos).

getJogosByGender(Connection, Gender, Jogos):-
    Q = "SELECT * FROM jogo WHERE LOWER(game_genero) = LOWER('%w')",
    db_parameterized_query(Connection, Q, [Gender], Jogos).

getJogosOrderByDate(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_data_lancamento DESC",
    db_query(Connection, Q, Jogos).

getJogosOrderByName(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_nome",
    db_query(Connection, Q, Jogos).

getJogosOrderByPrice(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_price",
    db_query(Connection, Q, Jogos).

getJogosOrderByBiggestPrice(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_price DESC",
    db_query(Connection, Q, Jogos).

getJogosOrderByRating(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_avaliacao DESC",
    db_query(Connection, Q, Jogos).

print_jogos([]) :-
    writeln('Nenhum jogo encontrado.').

print_jogos([Jogo|OutrosJogos]) :-
    writeln('================================================================================'),
    writeln('                            LISTA DE JOGOS                                    '),
    writeln('================================================================================'),
    print_jogo(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    print_jogos_rest(OutrosJogos).

print_jogos_rest([]).

print_jogos_rest([Jogo|OutrosJogos]) :-
    print_jogo(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    print_jogos_rest(OutrosJogos).

print_jogo(row(ID, Nome, Genero, _, _, _, Preco)) :-
    format('ID: ~d~nNome: ~w~nGênero: ~w~nPreço: ~2f~n', [ID, Nome, Genero, Preco]).

print_jogo_detalhado([]) :-
    writeln('Nenhum jogo encontrado.').

print_jogo_detalhado([Jogo|OutrosJogos]) :-
    print_jogo_detalhado_individual(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    print_jogo_detalhado(OutrosJogos).

print_jogo_detalhado_individual(row(ID, Nome, Genero, Descricao, date(Ano, Mes, Dia), Avaliacao, Preco)) :-
    writeln('================================================================================'),
    format('           DETALHES DO JOGO [~d]           ~n', [ID]),
    writeln('================================================================================'),
    format('ID: ~d~nNome: ~w~nGênero: ~w~nDescrição: ~w~nData de Lançamento: ~d/~d/~d~nAvaliação: ~1f~nPreço: ~2f~n', [ID, Nome, Genero, Descricao, Dia, Mes, Ano, Avaliacao, Preco]).



