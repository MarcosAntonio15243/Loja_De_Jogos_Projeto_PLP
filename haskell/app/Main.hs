module Main where

import LocalDB.ConnectionDB
import Controllers.JogoController

main :: IO ()
main = do
    putStrLn "Criando base de dados..."
    conn <- iniciandoDatabase
    putStrLn "Base de dados criada"
    jogo <- getJogoPorId conn 7
    printJogoDetalhado jogo
    

