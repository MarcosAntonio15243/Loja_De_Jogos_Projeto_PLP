:- module(compra,
    [temSaldo/4,
    naoComprouJogo/3,
    aptoParaComprar/3,
    realizaCompra/3,
    inserirCompra/3]).

:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- use_module("Jogo").
:- use_module(util, [printColorido/2]).
:- use_module(library(date)).

temSaldo(Connection, JogoId, UserId, true):-
    getPriceJogo(Connection, JogoId, PrecoJogo),
    getSaldoUsuario(Connection, UserId, SaldoUsuario),
    SaldoUsuario >= PrecoJogo.

temSaldo(Connection, JogoId, UserId, false):-
    getPriceJogo(Connection, JogoId, PrecoJogo),
    getSaldoUsuario(Connection, UserId, SaldoUsuario),
    SaldoUsuario < PrecoJogo.


naoComprouJogo(Connection, JogoId, UserId):-
    Q = "SELECT COUNT(*) FROM compra WHERE user_id = %w AND game_id = %w",
    db_parameterized_query(Connection, Q, [UserId, JogoId], [row(CountRow)]),
    (CountRow =< 0).


aptoParaComprar(Connection, JogoId, UserId):-
    temSaldo(Connection, JogoId, UserId, true),
    jogoExiste(Connection, JogoId),
    naoComprouJogo(Connection, JogoId, UserId).


inserirCompra(Connection, UserId, JogoId):-
    getPriceJogo(Connection, JogoId, PrecoDoJogo),

    get_time(TStamp),
    format_time(string(Txt),'%FT%T%z',TStamp),
    split_string(Txt, "T", "", DataSplitada),
    nth0(0, DataSplitada, DataFormatada),


    getSaldoUsuario(Connection, UserId, SaldoAtual),
    NovoSaldo is SaldoAtual - PrecoDoJogo,
    setSaldoUsuario(Connection, UserId, NovoSaldo),
    Q = "INSERT INTO compra (compra_data, compra_price, user_id, game_id) VALUES ('%w', %w, %w, %w)",
    db_parameterized_query_no_return(Connection, Q, [DataFormatada, PrecoDoJogo, UserId, JogoId]).


realizaCompra(Connection, JogoId, UserId) :-
    (   aptoParaComprar(Connection, JogoId, UserId)
    ->  inserirCompra(Connection, UserId, JogoId),
        printColorido('Compra realizada com sucesso!', green)
    ;   printColorido('Não foi possível realizar a compra. Verifique se o jogo existe, se você possui saldo suficiente para comprá-lo, ou se você já possui o jogo.', red)
    ).