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
                    \game_avaliacao INT NOT NULL,\
                    \game_price FLOAT NOT NULL);"
    return ()

iniciandoDatabase :: IO Connection
iniciandoDatabase = do
    c <- connectionMyDB
    createUsers c
    createGames c
    return c
    
    