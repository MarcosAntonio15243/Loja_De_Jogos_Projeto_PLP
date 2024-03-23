{-# LANGUAGE OverloadedStrings #-}

module Controller.Util where
import Database.PostgreSQL.Simple
import Data.Int (Int64)
import Data.Maybe (listToMaybe)

import System.Console.ANSI
import System.Process (callCommand)
import System.Info (os)


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
    return ()

checarNicknameExistente::Connection->String->IO Bool
checarNicknameExistente conn nickname = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE user_nickname = ?" (Only nickname)
    return (count /= (0 :: Int))

checarEmailExistente::Connection->String->IO Bool
checarEmailExistente conn email = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE user_email = ?" (Only email)
    return (count /= (0 :: Int))


getNomeByID::Connection->Int64->IO (Maybe String)
getNomeByID conn id = do
    result <- query conn "SELECT user_nome FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only nome] -> return $ Just nome
        _ -> return Nothing


getIDByNickname::Connection->String->IO (Maybe Int64)
getIDByNickname conn nick = do
    result <- query conn "SELECT user_id FROM usuario WHERE user_nickname = ?" (Only nick) :: IO [Only Int64]
    case result of
        [Only user_id] -> return $ Just user_id
        _ -> return Nothing

getNickByID::Connection->Int64->IO (Maybe String)
getNickByID conn id = do
    result <- query conn "SELECT user_nickname FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only nickname] -> return $ Just nickname
        _ -> return Nothing

getSaldoByID:: Connection -> Int64 -> IO (Maybe Double)
getSaldoByID conn id = do
    result <- query conn "SELECT user_saldo FROM usuario WHERE user_id = ?" (Only id) :: IO [Only Double]
    case result of
        [Only saldo] -> return $ Just saldo
        _ -> return Nothing

addSaldoByID:: Connection -> Int64 -> Double -> IO()
addSaldoByID conn id saldoAdicionar = do

    maybeSaldoAtual <- getSaldoByID conn id --obtem o saldo atual
    case maybeSaldoAtual of
        Just saldoAtual -> do
            let novoSaldo = saldoAtual + saldoAdicionar -- calcula novo saldo
            _ <- execute conn "UPDATE usuario SET user_saldo = ? WHERE user_id = ?" (novoSaldo, id) -- atualiza saldo no bd
            putStrLn "============================================================"
            putStrLn "                 Saldo atualizado com sucesso!              "
            putStrLn "============================================================"
        Nothing -> do
            putStrLn "Não foi possível obter o saldo atual do usuário"

getSenhaByID:: Connection -> Int64 -> IO (Maybe String)
getSenhaByID conn id = do
    result <- query conn "SELECT user_senha FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only senha] -> return $ Just senha
        _ -> return Nothing