{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)

-- import System.Process (callCommand)
-- import System.Info (os)

menuConta::Connection->Int64->Int64->IO()
menuConta conn user_id friend_id = do
    putStrLn "================================================================================"
    putStrLn "                                     CHAT                                       "
    putStrLn "================================================================================"
    putStrLn ""

    maybeDestinatario <- getNomeByID conn friend_id
    case (maybeDestinatario) of
        Just (destinatario) -> do
            mensagens <- getMensagens conn user_id friend_id
            exibeMensagens user_id destinatario mensagens
        Nothing -> do putStrLn "Id amigo Inválido!"
    
    -- putStrLn (show remetente)

    -- putStrLn ("\ESC[91m[Fulano]:\ESC[0m "++"Bom dia!\n")
    -- putStrLn ("\ESC[92m[Você]:\ESC[0m "++"Boa Tarde! Esta é uma mensagem que deve quebrar no tamanho do chat\n")
    
getNomeByID :: Connection -> Int64 -> IO (Maybe String)
getNomeByID conn id = do
    result <- query conn "SELECT user_nome FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only nome] -> return $ Just nome
        _ -> return Nothing

exibeMensagens::Int64->String->[(Int64, String)]->IO()
exibeMensagens _ _ [] = putStrLn "================================================================================"
exibeMensagens user_id destinatario ((id_remetente, message_texto):t) = do
    if (id_remetente == user_id) then do
        putStrLn ("\ESC[92m[Você]:\ESC[0m " ++ message_texto ++ "\n")
    else do
        putStrLn ("\ESC[91m[" ++ destinatario ++ "]:\ESC[0m " ++ message_texto ++ "\n")
    exibeMensagens user_id destinatario t

getMensagens::Connection->Int64->Int64->IO [(Int64, String)]
getMensagens conn user_id friend_id = do
    mensagens <- query conn "SELECT id_remetente, message_texto FROM mensagem \
            \WHERE id_remetente IN (?, ?) AND id_destinatario IN (?,?) \
            \ORDER BY message_date" (user_id, friend_id, user_id, friend_id)
    return [(id_remetente, message_texto) | (id_remetente, message_texto) <- mensagens]
    
