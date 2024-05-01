:- set_prolog_flag(encoding, utf8).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB", [
    iniciandoDatabase/1,
    encerrandoDatabase/1
]).
:- use_module("Controller/User").

main :-
    writeln('Iniciando database...'),
    iniciandoDatabase(Connection),
    limparTela,
    menuInicial,
    %limparTela,
    writeln("SAIU"),
    encerrandoDatabase(Connection).