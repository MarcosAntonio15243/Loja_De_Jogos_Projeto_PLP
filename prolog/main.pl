:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).
:- use_module(util).
:- use_module(cliente).
:- use_module("./LocalDB/ConnectionDB", [
    iniciandoDatabase/1,
    encerrandoDatabase/1
]).
:- use_module("Controller/User").

:- initialization(main).

/* Inicialização do programa */
main :-
    writeln('Iniciando database...'),
    iniciandoDatabase(Connection),
    %limparTela,
    menuInicial,
    encerrandoDatabase(Connection),
    halt.