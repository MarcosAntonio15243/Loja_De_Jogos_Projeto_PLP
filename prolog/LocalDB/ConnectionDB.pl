:- module(connectiondb, [
    iniciandoDatabase/1,
    encerrandoDatabase/1,
    createUsers/1,
    createGames/1,
    createCompras/1,
    createMensagens/1,
    createComentarios/1,
    createDenuncias/1
]).

:- use_module(library(odbc)).

iniciandoDatabase(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).

encerrandoDatabase(Connection) :-
    odbc_disconnect(Connection).

createUsers(Connection) :-
    odbc_query(Connection,
        "CREATE TABLE IF NOT EXISTS usuario (
            user_id SERIAL PRIMARY KEY,
            user_nickname VARCHAR(50) NOT NULL,
            user_nome VARCHAR(50) NOT NULL,
            user_email VARCHAR(50) NOT NULL,
            user_senha VARCHAR(50) NOT NULL,
            user_tipo VARCHAR(50) NOT NULL,
            user_date DATE NOT NULL,
            user_saldo FLOAT NOT NULL
        );", _).

createGames(Connection) :-
    odbc_query(Connection, "
        CREATE TABLE IF NOT EXISTS jogo (
            game_id SERIAL PRIMARY KEY,
            game_nome VARCHAR(50) NOT NULL,
            game_genero VARCHAR(50) NOT NULL,
            game_description TEXT NOT NULL,
            game_data_lancamento DATE NOT NULL,
            game_avaliacao FLOAT NOT NULL,
            game_price FLOAT NOT NULL
        );", _).

createCompras(Connection) :-
    odbc_query(Connection, "
        CREATE TABLE IF NOT EXISTS compra (
            compra_id SERIAL PRIMARY KEY,
            compra_data DATE NOT NULL,
            compra_price FLOAT NOT NULL,
            user_id INT NOT NULL,
            game_id INT NOT NULL,
            avaliacao_compra INT DEFAULT (-1),
            favoritar_jogo BOOLEAN DEFAULT (false),
            FOREIGN KEY (user_id) REFERENCES usuario(user_id) ON DELETE CASCADE,
            FOREIGN KEY (game_id) REFERENCES jogo(game_id) ON DELETE CASCADE
        );", _).

createMensagens(Connection) :-
    odbc_query(Connection, "
        CREATE TABLE IF NOT EXISTS mensagem (
            message_id SERIAL PRIMARY KEY,
            id_remetente INT NOT NULL,
            id_destinatario INT NOT NULL,
            message_texto TEXT NOT NULL,
            message_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
            FOREIGN KEY (id_remetente) REFERENCES usuario(user_id) ON DELETE CASCADE,
            FOREIGN KEY (id_destinatario) REFERENCES usuario(user_id) ON DELETE CASCADE
        );", _).

createComentarios(Connection) :-
    odbc_query(Connection, "
        CREATE TABLE IF NOT EXISTS comentario (
            comentario_id SERIAL PRIMARY KEY,
            id_usuario INT NOT NULL,
            id_jogo INT NOT NULL,
            comentario_texto TEXT NOT NULL,
            comentario_date DATE NOT NULL,
            FOREIGN KEY (id_usuario) REFERENCES usuario(user_id) ON DELETE CASCADE,
            FOREIGN KEY (id_jogo) REFERENCES jogo(game_id) ON DELETE CASCADE
        );", _).

createDenuncias(Connection) :-
    odbc_query(Connection, "
        CREATE TABLE IF NOT EXISTS denuncia (
            denuncia_id SERIAL PRIMARY KEY,
            id_usuario INT NOT NULL,
            id_jogo INT NOT NULL,
            denuncia_motivo VARCHAR(50) NOT NULL,
            denuncia_descricao TEXT NOT NULL,
            denuncia_data DATE NOT NULL,
            FOREIGN KEY (id_usuario) REFERENCES usuario(user_id) ON DELETE CASCADE,
            FOREIGN KEY (id_jogo) REFERENCES jogo(game_id) ON DELETE CASCADE
        );", _).

teste(X):- write(X).