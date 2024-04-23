:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB", [
    iniciandoDatabase/1,
    encerrandoDatabase/1
]).
:- use_module("./Controller/Jogo").

main :-
    writeln('Iniciando database...'),
    iniciandoDatabase(Connection),
    Teste = jogoExiste(Connection, 3),
    writeln(Teste),
    writeln('Aqui será chamado a função que abre o menu inicial'),
    

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