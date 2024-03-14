{-# LANGUAGE OverloadedStrings #-}

module Controller.Util where
import Database.PostgreSQL.Simple
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import System.Console.ANSI

limparTela::IO()
limparTela = do
    clearScreen
    setCursorPosition 0 0

apagarLinha::IO()
apagarLinha = do
    clearLine
    setCursorColumn 0

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