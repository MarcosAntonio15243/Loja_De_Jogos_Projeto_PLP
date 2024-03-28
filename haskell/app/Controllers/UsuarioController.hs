{-# LANGUAGE OverloadedStrings #-}

module Controllers.UsuarioController where
import Database.PostgreSQL.Simple
import Models.Usuario
import Control.Monad (void)


getSaldoUsuario :: Connection -> Int -> IO Double
getSaldoUsuario conn userId = do
    [Only saldo] <- query conn "SELECT user_saldo FROM usuario WHERE user_id = ?" (Only userId)
    return saldo


setSaldoUsuario :: Connection -> Int -> Double -> IO ()
setSaldoUsuario conn idUser novoValor = void $ do
    execute conn "UPDATE usuario SET user_saldo = ? WHERE user_id = ?" (novoValor, idUser)
