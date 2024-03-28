{-# LANGUAGE OverloadedStrings #-}

module Controller.CompraController where
import Database.PostgreSQL.Simple
import Models.Compra
import Models.Usuario
import Controller.JogoController
import Controller.UsuarioController
import Data.Time.Clock (getCurrentTime, utctDay)  
import Data.Time.Calendar (Day)

import Data.Int (Int64)


temSaldo :: Connection -> Int64 -> Int64 -> IO Bool
temSaldo conn idUser idJogo = do
    precoJogo <- getPrecoDoJogo conn idJogo
    saldoUser <- getSaldoUsuario conn idUser
    return (saldoUser >= precoJogo)


naoComprouJogo:: Connection -> Int64 -> Int64 -> IO Bool
naoComprouJogo conn idUser idJogo = do
    [Only count] <- query conn querySQL (idUser, idJogo)
    return (count == (0 :: Int))
  where
    querySQL = "SELECT COUNT(*) FROM compra WHERE user_id = ? AND game_id = ?"


realizaCompra :: Connection -> Int64 -> Int64 -> IO ()
realizaCompra conn idUser idJogo = do
    jogoExiste <- existeJogo conn idJogo
    podeComprar <- temSaldo conn idUser idJogo
    naoPossuiJogo <- naoComprouJogo conn idUser idJogo
    
    if jogoExiste && podeComprar && naoPossuiJogo
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
        else putStrLn "Não foi possível realizar a compra. Verifique se o jogo existe, se você possui saldo suficiente para comprá-lo, e se você já possui o jogo."