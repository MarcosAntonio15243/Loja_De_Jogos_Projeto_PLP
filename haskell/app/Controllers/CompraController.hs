{-# LANGUAGE OverloadedStrings #-}

module Controllers.CompraController where
import Database.PostgreSQL.Simple
import Models.Compra
import Models.Usuario
import Controllers.JogoController
import Controllers.UsuarioController
import Data.Time.Clock (getCurrentTime, utctDay)  
import Data.Time.Calendar (Day)

temSaldo :: Connection -> Int -> Int -> IO Bool
temSaldo conn idUser idJogo = do
    precoJogo <- getPrecoDoJogo conn idJogo
    saldoUser <- getSaldoUsuario conn idUser
    return (saldoUser >= precoJogo)


realizaCompra :: Connection -> Int -> Int -> IO ()
realizaCompra conn idUser idJogo = do
    jogoExiste <- existeJogo conn idJogo
    podeComprar <- temSaldo conn idUser idJogo
    
    if jogoExiste && podeComprar
        then do
            precoJogo <- getPrecoDoJogo conn idJogo

            currentDate <- getCurrentTime
            let currentDay = utctDay currentDate
            
            execute conn "INSERT INTO compra (compra_data, compra_price, user_id, game_id) VALUES (?, ?, ?, ?)"
                (currentDay, precoJogo, idUser, idJogo)

            saldoAtual <- getSaldoUsuario conn idUser
            let novoSaldo = saldoAtual - precoJogo
            setSaldoUsuario conn idUser novoSaldo

            putStrLn "Compra realizada com sucesso!"
        else putStrLn "Não foi possível realizar a compra. Verifique se o jogo existe e se você possui saldo suficiente para comprá-lo."