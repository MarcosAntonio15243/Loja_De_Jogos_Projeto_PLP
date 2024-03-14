{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)

import Controller.Util

menuCliente::Connection->Int64->IO()
menuCliente conn user_id = do
    putStrLn "================================================================================"
    putStrLn "                               PÁGINA PRINCIPAL                                 "
    putStrLn "================================================================================"
    maybeUser <- getNomeByID conn user_id
    case (maybeUser) of
        Just (nome) -> do
            putStrLn ("Seja bem vindo(a) \ESC[94m" ++ nome ++ "\ESC[0m!")
            putStrLn "O que deseja fazer?"
            putStrLn ""
            putStrLn "1 - Jogos Disponíveis"
            putStrLn "2 - Mensagens"
            putStrLn "3 - Meu Perfil"
            putStrLn "4 - Sair"
            putStrLn ""
            putStrLn "Selecione uma opção: "

            opcao <- getLine
            putStrLn "================================================================================"

            case opcao of
                "1" -> do putStrLn "A FAZER"
                "2" -> do
                    putStrLn "Digite o nickname do usuário que deseja enviar a mensagem:"
                    friend_nickname <- getLine
                    checarNicknameUserFriend conn user_id friend_nickname
                "3" -> do putStrLn "A FAZER"
                "4" -> return()
                _ -> do
                    putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
                    menuCliente conn user_id
        Nothing -> do putStrLn "Id usuário Inválido!"

        
checarNicknameUserFriend::Connection->Int64->String->IO()
checarNicknameUserFriend conn user_id friend_nickname = do
    maybe_friend_id <- getIDByNickname conn friend_nickname
    case maybe_friend_id of
         Just (friend_id) ->
            if (user_id == friend_id) then do
                limparTela
                putStrLn "O nickname digitado não pode ser seu próprio nick!"
                putStrLn "Tente novamente utilizando outro nickname."
                menuCliente conn user_id
            else
                abrirChat conn user_id friend_id friend_nickname
         Nothing -> do
            putStrLn "ID amigo não encontrado!"
            menuCliente conn user_id



abrirChat::Connection->Int64->Int64->String->IO()
abrirChat conn user_id friend_id friend_nickname = do
    limparTela
    putStrLn "================================================================================"
    putStrLn "                                     CHAT                                       "
    putStrLn "================================================================================"
    putStrLn ""
    mensagens <- getMensagens conn user_id friend_id
    exibeMensagens user_id friend_nickname mensagens
    putStrLn "================================================================================"
    apagarLinha
    putStrLn "Escreva uma mensagem (ou tecle ENTER para sair):"
    mensagem <- getLine
    
    if (Prelude.null mensagem) then do
        menuCliente conn user_id
        limparTela
    else do
        enviarMensagem conn user_id friend_id mensagem
        limparTela
        abrirChat conn user_id friend_id friend_nickname


enviarMensagem::Connection->Int64->Int64->String->IO()
enviarMensagem conn user_id friend_id mensagem = do
    let q = "INSERT INTO mensagem\
                    \(id_remetente, \
                    \id_destinatario, \
                    \message_texto) \
                    \values (?, ?, ?)"
    execute_ conn "BEGIN"
    _ <- execute conn q (user_id, friend_id, mensagem)
    execute_ conn "COMMIT"
    return()


getMensagens::Connection->Int64->Int64->IO [(Int64, String)]
getMensagens conn user_id friend_id = do
    mensagens <- query conn "SELECT id_remetente, message_texto FROM mensagem \
            \WHERE id_remetente IN (?, ?) AND id_destinatario IN (?,?) \
            \ORDER BY message_date" (user_id, friend_id, user_id, friend_id)
    return [(id_remetente, message_texto) | (id_remetente, message_texto) <- mensagens]


exibeMensagens::Int64->String->[(Int64, String)]->IO()
exibeMensagens _ _ [] = return()
exibeMensagens user_id friend_nickname ((id_remetente, message_texto):t) = do
    if (id_remetente == user_id) then do
        putStrLn ("\ESC[92m[Você]:\ESC[0m " ++ message_texto ++ "\n")
    else do
        putStrLn ("\ESC[91m[" ++ friend_nickname ++ "]:\ESC[0m " ++ message_texto ++ "\n")
    exibeMensagens user_id friend_nickname t


    
