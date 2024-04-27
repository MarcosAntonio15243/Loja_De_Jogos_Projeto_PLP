:- module(compra,
    [temSaldo/4]).

:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- use_module("Usuario").
:- use_module("Jogo").

temSaldo(Connection, JogoId, UserId, true):-
    getPriceJogo(Connection, JogoId, PrecoJogo),
    getSaldoUsuario(Connection, UserId, SaldoUsuario),
    SaldoUsuario >= PrecoJogo.

temSaldo(Connection, JogoId, UserId, false):-
    getPriceJogo(Connection, JogoId, PrecoJogo),
    getSaldoUsuario(Connection, UserId, SaldoUsuario),
    SaldoUsuario < PrecoJogo.


