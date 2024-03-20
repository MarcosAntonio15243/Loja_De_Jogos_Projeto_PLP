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
    
    putStrLn "================================================================================"
    putStrLn "                     OBRIGADO POR UTILIZAR O NOSSO SISTEMA!                     "
    putStrLn "================================================================================"
