module Main where

import LocalDB.ConnectionDB
import Controllers.JogoController

main :: IO ()
main = do
    putStrLn "Criando base de dados..."
    conn <- iniciandoDatabase
    putStrLn "Base de dados criada"
    jogos <- getJogos conn
    printJogos jogos
    idJogo <- getLine
    let jogoId = read idJogo :: Integer
    jogo <- getJogoPorId conn jogoId 
    printJogoDetalhado jogo
    

