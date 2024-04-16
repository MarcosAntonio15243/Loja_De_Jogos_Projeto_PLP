:- set_prolog_flag(encoding, utf8).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB", [
    iniciandoDatabase/1,
    encerrandoDatabase/1
]).

main :-
    writeln('Iniciando database...'),
    iniciandoDatabase(Connection),
    
    limparTela,
    writeln('Aqui será chamado a função que abre o menu inicial'),
    limparTela,

    encerrandoDatabase(Connection),

    writeln('╔══════════════════════════════════════════════════════════════════════════════╗'),
    writeln('║                                                                              ║'),
    writeln('║                    OBRIGADO POR UTILIZAR O NOSSO SISTEMA!                    ║'),
    writeln('║                                                                              ║'),
    writeln('║══════════════════════════════════════════════════════════════════════════════║'),
    writeln('║                                                                              ║'),
    writeln('║                                NOSSA EQUIPE:                                 ║'),
    writeln('║                                                                              ║'),
    writeln('║                                HILDON REGIS                                  ║'),
    writeln('║                                LEILA FARIAS                                  ║'),
    writeln('║                               MARCOS ANTONIO                                 ║'),
    writeln('║                               MARCOS VINÍCIUS                                ║'),
    writeln('║                                 JOÃO VICTOR                                  ║'),
    writeln('║                                                                              ║'),
    writeln('╚══════════════════════════════════════════════════════════════════════════════╝'),

    halt.