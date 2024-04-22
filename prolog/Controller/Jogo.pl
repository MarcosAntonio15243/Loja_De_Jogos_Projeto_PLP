:- module(jogo, [getJogos/2, getJogosById/3]).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").


getJogos(Connection, Jogo):-
    Q = "SELECT * FROM jogo",
    db_query(Connection, Q, Jogo).


getJogosById(Connection, jogoId, Jogo):-
    Q = "SELECT * FROM jogo WHERE game_id = %w",
    db_parameterized_query(Connection, Q, [jogoId], Jogo).