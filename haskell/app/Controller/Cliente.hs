{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)
import Data.Maybe (fromJust)
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
                "3" -> do perfilCliente conn user_id
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


perfilCliente:: Connection -> Int64 -> IO()
perfilCliente conn userID = do
    
    maybeNickname <- getNickByID conn userID
    case maybeNickname of
        Just nickname -> do
            putStrLn "============================================================"
            putStrLn ("\ESC[94m" ++ nickname ++ "\ESC[0m" ++ "'s Perfil      ")
            putStrLn ""
            putStrLn "1 - Meus jogos"   
            putStrLn "2 - Minha carteira"
            putStrLn "3 - Editar perfil"
            putStrLn "4 - Voltar"   
            putStrLn ""
            putStrLn "============================================================"
            putStrLn "Selecione uma opção > "

            opcao <- getLine
            limparTela
            
            case opcao of
                "1" -> do
                    putStrLn "A FAZER\n"
                "2" -> do
                    carteiraCliente conn userID
                "3" -> do
                    putStrLn "A FAZER\n"
                "4" -> do
                    limparTela
                    menuCliente conn userID
                _ -> do
                    putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
                    perfilCliente conn userID

        Nothing -> do putStrLn "ID Usuário Inválido"
    

carteiraCliente:: Connection -> Int64 -> IO ()
carteiraCliente conn userID = do

    saldo <- getSaldoByID conn userID

    putStrLn "============================================================"
    putStrLn "                      Sua Carteira                          "
    putStrLn "============================================================"
    putStrLn ""
    putStrLn "     ╔══════════════════════════════════════════════╗"
    putStrLn "     ║                                              ║"
    putStrLn $ "       Saldo Disponível: R$" ++ show (fromJust saldo) 
    putStrLn "     ║                                              ║"
    putStrLn "       1. Adicionar Saldo                           "
    putStrLn "     ║ 2. Voltar                                    ║"
    putStrLn "     ║                                              ║"
    putStrLn "     ╚══════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione uma opção > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> do
            selecionarQntSaldo conn userID
        "2" -> do
            perfilCliente conn userID
        _ -> do
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            carteiraCliente conn userID


selecionarQntSaldo:: Connection -> Int64 -> IO()
selecionarQntSaldo conn userID = do
    putStrLn "============================================================"
    putStrLn " Quanto deseja adicionar?                                   "
    putStrLn ""
    putStrLn "1 - R$ 5,00"   
    putStrLn "2 - R$ 10,00"
    putStrLn "3 - R$ 20,00"
    putStrLn "4 - R$ 50,00"
    putStrLn "5 - R$ 100,00"
    putStrLn "6 - Voltar"   
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione uma opção > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> selecionarSaldo conn userID 5.00
        "2" -> selecionarSaldo conn userID 10.00
        "3" -> selecionarSaldo conn userID 20.00
        "4" -> selecionarSaldo conn userID 50.00
        "5" -> selecionarSaldo conn userID 100.00
        "6" -> carteiraCliente conn userID      

        _ -> do
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            selecionarQntSaldo conn userID

solicitarSenha :: Connection -> Int64 -> IO Bool
solicitarSenha conn userID = do
    putStrLn "============================================================"
    putStrLn "Confirme sua senha: "
    senhaDigitada <- getLine
            
    maybeSenhaReal <- getSenhaByID conn userID
    case maybeSenhaReal of
        Just senhaReal -> do
            if senhaReal == senhaDigitada then
                return True
            else do
                putStrLn "============================================================"
                putStrLn "\ESC[91mSENHA INCORRETA\ESC[0m"
                putStrLn "============================================================"
                putStrLn "1. Tentar novamente"
                putStrLn "2. Cancelar"
                putStrLn "============================================================"
                putStrLn "Selecione uma opção > "

                opcao <- getLine
                limparTela

                case opcao of
                    "1" -> solicitarSenha conn userID
                    "2" -> return False
                    _ -> putStrLn "\ESC[91mOpção inválida!\ESC[0m" >> return False

        Nothing -> do
            putStrLn "ERRO: SENHA NÃO ENCONTRADA" >> return False

selecionarSaldo :: Connection -> Int64 -> Double -> IO ()
selecionarSaldo conn userID saldo = do
    confirmarSenha <- solicitarSenha conn userID

    if confirmarSenha
        then do addSaldoByID conn userID saldo
                carteiraCliente conn userID
    else do
        selecionarQntSaldo conn userID
        
                


    

