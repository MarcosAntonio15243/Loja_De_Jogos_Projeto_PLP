module Main where

import Controller.User
import Controller.Cliente
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB

import Controller.Util

main :: IO ()
main = do
    putStrLn "Criando base de dados..."
    conn <- iniciandoDatabase

    limparTela
    menuInicial conn
    limparTela
    
    putStrLn "╔══════════════════════════════════════════════════════════════════════════════╗"
    putStrLn ""
    putStrLn "║                    OBRIGADO POR UTILIZAR O NOSSO SISTEMA!                    ║"
    putStrLn "║                                                                              ║"
    putStrLn "║══════════════════════════════════════════════════════════════════════════════║"
    putStrLn "║                                                                              ║"
    putStrLn "║                                NOSSA EQUIPE:                                 ║"
    putStrLn "║                                                                              ║"
    putStrLn "║                                HILDON REGIS                                  ║"
    putStrLn "║                                LEILA FARIAS                                  ║"
    putStrLn "║                               MARCOS ANTONIO                                 ║"
    putStrLn "║                               MARCOS VINÍCIUS                                ║"
    putStrLn "║                                                                              ║"
    putStrLn "╚══════════════════════════════════════════════════════════════════════════════╝"
