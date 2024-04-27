:- module(usuario,
    [getSaldoUsuario/3,
    setSaldoUsuario/3]).

:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").


getSaldoUsuario(Connection, UserId, Saldo):-
    Q = "SELECT user_saldo FROM usuario WHERE user_id = %w",
    db_parameterized_query(Connection, Q, [UserId], SaldoRow),
    getUniqueDataRow(SaldoRow, Saldo).


setSaldoUsuario(Connection, UserId, NovoValor):-
    Q = "UPDATE usuario SET user_saldo = %w WHERE user_id = %w",
    db_parameterized_query_no_return(Connection, Q, [NovoValor, UserId]).