{-# LANGUAGE OverloadedStrings #-}
module LocalDB.ConnectionDB where
import Database.PostgreSQL.Simple

localDB:: ConnectInfo
localDB = defaultConnectInfo {
    connectHost = "localhost",
    connectDatabase = "lojajogos",
    connectUser = "postgres",
    connectPassword = "postgres",
    connectPort = 5432
}

connectionMyDB :: IO Connection
connectionMyDB = connect localDB


createUsers :: Connection -> IO()
createUsers conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS usuario (\
                    \user_id SERIAL PRIMARY KEY,\
                    \user_nickname VARCHAR(50) NOT NULL,\
                    \user_nome VARCHAR(50) NOT NULL,\
                    \user_email VARCHAR(50) NOT NULL,\
                    \user_senha VARCHAR(50) NOT NULL,\
                    \user_tipo VARCHAR(50) NOT NULL,\
                    \user_date DATE NOT NULL,\
                    \user_saldo FLOAT NOT NULL);"
    return ()



createGames :: Connection -> IO()
createGames conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS jogo (\
                    \game_id SERIAL PRIMARY KEY,\
                    \game_nome VARCHAR(50) NOT NULL,\
                    \game_genero VARCHAR(50) NOT NULL,\
                    \game_description TEXT NOT NULL,\
                    \game_data_lancamento DATE NOT NULL,\
                    \game_avaliacao FLOAT NOT NULL,\
                    \game_price FLOAT NOT NULL);"
    return ()

createCompras :: Connection -> IO()
createCompras conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS compra (\
                    \compra_id SERIAL PRIMARY KEY,\
                    \compra_data DATE NOT NULL,\
                    \compra_price FLOAT NOT NULL,\
                    \user_id INT NOT NULL,\
                    \game_id INT NOT NULL,\
                    \avaliacao_compra INT DEFAULT (-1),\
                    \favoritar_jogo BOOLEAN DEFAULT (false),\
                    \FOREIGN KEY (user_id) REFERENCES usuario(user_id) ON DELETE CASCADE,\
                    \FOREIGN KEY (game_id) REFERENCES jogo(game_id) ON DELETE CASCADE);"
    return ()

createMensagens :: Connection -> IO()
createMensagens conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS mensagem (\
                    \message_id SERIAL PRIMARY KEY,\
                    \id_remetente INT NOT NULL,\
                    \id_destinatario INT NOT NULL,\
                    \message_texto TEXT NOT NULL,\
                    \message_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,\
                    \FOREIGN KEY (id_remetente) REFERENCES usuario(user_id) ON DELETE CASCADE,\
                    \FOREIGN KEY (id_destinatario) REFERENCES usuario(user_id) ON DELETE CASCADE);"
    return ()

createComentarios :: Connection -> IO()
createComentarios conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS comentario (\
                    \comentario_id SERIAL PRIMARY KEY,\
                    \id_usuario INT NOT NULL,\
                    \id_jogo INT NOT NULL,\
                    \comentario_texto TEXT NOT NULL,\
                    \comentario_date DATE NOT NULL,\
                    \FOREIGN KEY (id_usuario) REFERENCES usuario(user_id) ON DELETE CASCADE,\
                    \FOREIGN KEY (id_jogo) REFERENCES jogo(game_id) ON DELETE CASCADE);"
    return ()

createDenuncias :: Connection -> IO()
createDenuncias conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS denuncia (\
                    \denuncia_id SERIAL PRIMARY KEY,\
                    \id_usuario INT NOT NULL,\
                    \id_jogo INT NOT NULL,\
                    \denuncia_motivo VARCHAR(50) NOT NULL,\
                    \denuncia_descricao TEXT NOT NULL,\
                    \denuncia_data DATE NOT NULL,\
                    \FOREIGN KEY (id_usuario) REFERENCES usuario(user_id) ON DELETE CASCADE,\
                    \FOREIGN KEY (id_jogo) REFERENCES jogo(game_id) ON DELETE CASCADE);"
    return ()

iniciandoDatabase :: IO Connection
iniciandoDatabase = do
    c <- connectionMyDB
    createUsers c
    createGames c
    createCompras c
    createMensagens c
    createComentarios c
    createDenuncias c
    return c