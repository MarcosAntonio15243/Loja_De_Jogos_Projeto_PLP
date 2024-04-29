:- module(util, [
    limparTela/0,
    verificaCamposNaoVazios/1,
    printColorido/2,
    verificaCamposTamMaxStr50/1
]).

:- use_module(library(process)).

limparTela :-
    current_prolog_flag(windows, true),
    process_create(path(cmd), ['/C', 'cls'], [process(PID)]),
    process_wait(PID, _), !.

/* Recebe uma lista de parâmetros String e verifica se algum deles é vazio */
verificaCamposNaoVazios([]) :- !.
verificaCamposNaoVazios([H|T]) :- H \== "", verificaCamposNaoVazios(T).

/* Recebe uma lista de parâmetros String e verifica se possuem tamanho máximo de 50 caracteres */
verificaCamposTamMaxStr50([]) :- !.
verificaCamposTamMaxStr50([H|T]) :- string_length(H, L), L =< 50, verificaCamposTamMaxStr50(T).

printColorido(Text, green) :- ansi_format([fg(green)], '~w', [Text]), writeln(""), !.
printColorido(Text, red) :- ansi_format([fg(235, 0, 0)], '~w', [Text]), writeln(""), !.
printColorido(Text, yellow) :- ansi_format([fg(yellow)], '~w', [Text]), writeln(""), !.
printColorido(Text, cyan) :- ansi_format([fg(cyan)], '~w', [Text]), writeln(""), !.