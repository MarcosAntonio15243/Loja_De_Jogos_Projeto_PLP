module Main where

import LocalDB.ConnectionDB
import Controllers.JogoController
import Controllers.UsuarioController
import Controllers.CompraController
import System.Process (callCommand)
import System.Info (os)
import System.Console.ANSI

limparTela::IO()
limparTela = do
    case os of
        "linux" -> callCommand "clear"
        "darwin" -> callCommand "clear"
        "mingw32" -> callCommand "cls"
        _ -> limparTelaANSI


limparTelaANSI::IO()
limparTelaANSI = do
    clearScreen
    setCursorPosition 0 0
    return()



main :: IO ()
main = do
    putStrLn "Criando base de dados..."
    conn <- iniciandoDatabase
    putStrLn "Base de dados criada"
    jogos <- getJogos conn
    printJogos jogos
    idJogo <- getLine
    let jogoId = read idJogo :: Int
    jogo <- getJogoPorId conn jogoId 
    limparTela
    printJogoDetalhado jogo
    realizaCompra conn 3 10
    

