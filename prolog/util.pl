:- module(util, [
    limparTela/0
]).
:- use_module(library(process)). 

limparTela :-
    current_prolog_flag(windows, true), 
    
    process_create(path(cmd), ['/C', 'cls'], [process(PID)]),
    
    process_wait(PID,_), !.
