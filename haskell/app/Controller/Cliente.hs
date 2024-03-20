{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)

import Controller.Util

import Controller.JogoController

menuCliente::Connection->Int64->IO()
menuCliente conn user_id = do
    putStrLn "================================================================================"
    putStrLn "                                      HOME                                      "
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
            putStrLn "================================================================================"
            putStrLn "Selecione uma opção: "

            opcao <- getLine
            limparTela

            case opcao of
                "1" -> do putStrLn "A FAZER"
                "2" -> do mensagens conn user_id
                "3" -> do putStrLn "A FAZER"
                "4" -> do
                    return()
                _ -> do
                    putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
                    menuCliente conn user_id
        Nothing -> do putStrLn "Id usuário Inválido!"


mensagens::Connection->Int64->IO()
mensagens conn user_id = do
    putStrLn "================================================================================"
    putStrLn "Digite o nickname do usuário para quem deseja enviar a mensagem:"
    friend_nickname <- getLine
    maybe_friend_id <- getIDByNickname conn friend_nickname
    putStrLn "================================================================================"
    case maybe_friend_id of
         Just (friend_id) ->
            if (user_id == friend_id) then do
                putStrLn "O nickname digitado não pode ser seu próprio nick!"
                putStrLn "Tente novamente utilizando outro nickname."
                desejaContinuar conn user_id (mensagens)
            else
                abrirChat conn user_id friend_id friend_nickname
         Nothing -> do
            putStrLn "Não existe usuário cadastrado com esse nickname!"
            desejaContinuar conn user_id (mensagens)

desejaContinuar::Connection->Int64->(Connection->Int64->IO())->IO()
desejaContinuar conn user_id funcao = do
    putStrLn "Deseja continuar? (s/n)"
    opcao <- getLine
    if (opcao == "s" || opcao == "S") then do
        limparTela
        funcao conn user_id
    else if (opcao == "n" || opcao == "N") then do
        limparTela
        menuCliente conn user_id
    else do
        putStrLn "Opção Inválida!"
        desejaContinuar conn user_id funcao


abrirChat::Connection->Int64->Int64->String->IO()
abrirChat conn user_id friend_id friend_nickname = do
    limparTela
    putStrLn "================================================================================"
    putStrLn "                                     CHAT                                       "
    putStrLn "================================================================================"
    mensagens <- getMensagens conn user_id friend_id
    if (length mensagens == 0) then do
        putStrLn "Sem mensagens entre esse usuário"
    else do
        putStrLn ""
        exibeMensagens user_id friend_nickname mensagens
    putStrLn "================================================================================"
    putStrLn "Escreva uma mensagem (ou tecle ENTER para sair):"
    mensagem <- getLine
    
    if (Prelude.null mensagem) then do
        limparTela
        menuCliente conn user_id
    else do
        enviarMensagem conn user_id friend_id mensagem
        limparTela
        abrirChat conn user_id friend_id friend_nickname

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


