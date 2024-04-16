:- module(databaseOperations, [
    get_connection/1,
    db_parameterized_query/4,
    db_parameterized_query_no_return/3,
    userAlreadyExistsById/2,
    userAlreadyExistsByNickname/2,
    userAlreadyExistsByEmail/2
]).
:- use_module(library(odbc)).

/* Busca a conexão com o banco de dados */
get_connection(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).

db_query(Connection, Query, Rows) :-
    findall(
        Result,
        odbc_query(Connection, Query, Result),
        Rows
    ).

/*
 Formata os paramêtros das queries para que sejam aceitas nas consultas com o banco de dados
*/
formatParamters([], []) :- !.
formatParamters([H|T1], [NewH|T2]) :-
    format(atom(NewH), "'~w'", [H]),
    formatParamters(T1, T2).

db_query_no_return(Connection, Query) :-
    odbc_query(Connection, Query).

db_parameterized_query(Connection, Query, Parameters, Rows):-
    formatParamters(Parameters, FormatedParamters),
    swritef(String, Query, FormatedParamters),
    db_query(Connection, String, Rows).
    
db_parameterized_query_no_return(Connection, Query, Parameters):-
    formatParamters(Parameters, FormatedParamters),
    swritef(String, Query, FormatedParamters),
    db_query_no_return(Connection, String).


userAlreadyExistsById(Connection, Id) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_id = %w",
    db_parameterized_query(Connection, Q, [Id], [row(CountRow)]),
    (CountRow > 0).

userAlreadyExistsByNickname(Connection, Nickname) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_nickname = %w",
    db_parameterized_query(Connection, Q, [Nickname], [row(CountRow)]),
    (CountRow > 0).

userAlreadyExistsByEmail(Connection, Email) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_email = %w",
    db_parameterized_query(Connection, Q, [Email], [row(CountRow)]),
    (CountRow > 0).