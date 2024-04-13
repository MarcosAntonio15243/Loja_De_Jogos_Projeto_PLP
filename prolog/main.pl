:- set_prolog_flag(encoding, utf8).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB", [
    iniciandoDatabase/1,
    encerrandoDatabase/1,
    createUsers/1,
    createGames/1,
    createCompras/1,
    createMensagens/1,
    createComentarios/1,
    createDenuncias/1
]).

% Coloquei os creates aqui mas não sei se fica agradável deixá-los assim
main :-
    writeln('Iniciando database...'),
    iniciandoDatabase(Connection),
    createUsers(Connection),
    createGames(Connection),
    createCompras(Connection),
    createMensagens(Connection),
    createComentarios(Connection),
    createDenuncias(Connection),
    
    limparTela,
    writeln('Aqui será chamado a função que abre o menu inicial'),
    limparTela,

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