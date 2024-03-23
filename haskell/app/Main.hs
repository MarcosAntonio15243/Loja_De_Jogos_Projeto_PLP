module Main where

import Controller.User
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB

main :: IO ()
main = do
    putStrLn "Criando base de dados..."
    conn <- iniciandoDatabase

    putStrLn "============================================================"
    putStrLn "                BEM VINDO(A) À LOJA DE JOGOS                "

    menuInicial conn

    putStrLn "============================================================"
    putStrLn "            OBRIGADO POR UTILIZAR O NOSSO SISTEMA!          "
    putStrLn "============================================================"

    putStrLn "Base de dados criada"