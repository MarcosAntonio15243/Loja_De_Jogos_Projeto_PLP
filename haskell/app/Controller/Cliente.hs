{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)

import System.Process (callCommand)
import System.Info (os)

menuConta::Connection->Int64->IO()
menuConta conn user_id = do
    putStrLn "================================================================================"
    putStrLn "                               PÁGINA PRINCIPAL                                 "
    putStrLn "================================================================================"
    maybeUser <- getNomeByID conn user_id
    case (maybeUser) of
        Just (nome) -> do
            putStrLn ("Seja bem vindo(a) \ESC[94m" ++ nome ++ "\ESC[0m!")
            putStrLn "O que deseja fazer?"
            putStrLn ""
            putStrLn "1 - Comprar Jogos"
            putStrLn "2 - Amigos"
            putStrLn "3 - Sair"
            putStrLn ""
            putStrLn "Selecione uma opção > "

            opcao <- getLine

            case opcao of
                "1" -> do
                    menuConta conn user_id
                "2" -> do
                    putStrLn "TESTES"
                    friend_nickname <- getLine -- Recebendo o nick do amigo
                    abrirChat conn user_id friend_nickname
                "3" -> return () -- alterar depois
                _ -> do
                    putStrLn "Opção inválida! Por favor, tente novamente."
                    menuConta conn user_id
        Nothing -> do putStrLn "Id usuário Inválido!"
    

getIDByNickname::Connection->String->IO (Maybe Int64)
getIDByNickname conn nick = do
    result <- query conn "SELECT user_id FROM usuario WHERE user_nickname = ?" (Only nick) :: IO [Only Int64]
    case result of
        [Only user_id] -> return $ Just user_id
        _ -> return Nothing

abrirChat::Connection->Int64->String->IO()
abrirChat conn user_id friend_nickname = do
    putStrLn "================================================================================"
    putStrLn "                                     CHAT                                       "
    putStrLn "================================================================================"
    putStrLn ""

    maybeFriendID <- getIDByNickname conn friend_nickname
    case (maybeFriendID) of
        Just (friend_id) -> do
            mensagens <- getMensagens conn user_id friend_id
            exibeMensagens user_id friend_nickname mensagens
        Nothing -> do putStrLn "Id amigo Inválido!"
    
getNomeByID::Connection->Int64->IO (Maybe String)
getNomeByID conn id = do
    result <- query conn "SELECT user_nome FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only nome] -> return $ Just nome
        _ -> return Nothing

exibeMensagens::Int64->String->[(Int64, String)]->IO()
exibeMensagens _ _ [] = putStrLn "================================================================================"
exibeMensagens user_id friend_nickname ((id_remetente, message_texto):t) = do
    if (id_remetente == user_id) then do
        putStrLn ("\ESC[92m[Você]:\ESC[0m " ++ message_texto ++ "\n")
    else do
        putStrLn ("\ESC[91m[" ++ friend_nickname ++ "]:\ESC[0m " ++ message_texto ++ "\n")
    exibeMensagens user_id friend_nickname t

getMensagens::Connection->Int64->Int64->IO [(Int64, String)]
getMensagens conn user_id friend_id = do
    mensagens <- query conn "SELECT id_remetente, message_texto FROM mensagem \
            \WHERE id_remetente IN (?, ?) AND id_destinatario IN (?,?) \
            \ORDER BY message_date" (user_id, friend_id, user_id, friend_id)
    return [(id_remetente, message_texto) | (id_remetente, message_texto) <- mensagens]
    
