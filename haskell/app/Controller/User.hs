{-# LANGUAGE OverloadedStrings #-}
module Controller.User where
import Controller.Cliente
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64)
import Data.Maybe (listToMaybe)

import Controller.Util
import Controller.Cliente

menuInicial::Connection -> IO()
menuInicial conn = do
    putStrLn "================================================================================"
    putStrLn "MENU:"
    putStrLn ""
    putStrLn "1 - Login"
    putStrLn "2 - Criar uma conta"
    putStrLn "3 - Sair"
    putStrLn ""
    putStrLn "================================================================================"
    putStrLn "Selecione uma opção: "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> do
            login conn
        "2" -> do
            criarConta conn
        "3" -> return ()
        _ -> do
            putStrLn "Opção inválida! Por favor, tente novamente."
            menuInicial conn

desejaContinuar::Connection->(Connection->IO())->IO()
desejaContinuar conn funcao = do
    putStrLn "Deseja continuar? (s/n)"
    opcao <- getLine
    if (opcao == "s" || opcao == "S") then do
        limparTela
        funcao conn
    else if (opcao == "n" || opcao == "N") then do
        limparTela
        menuInicial conn
    else do
        putStrLn "Opção Inválida!"
        desejaContinuar conn funcao

login::Connection->IO()
login conn = do
    putStrLn "================================================================================"
    putStrLn "                                       LOGIN                                    "
    putStrLn "================================================================================"
    putStrLn "Preencha seus dados abaixo:\n"
    putStrLn "Digite o e-mail:"
    email <- getLine
    putStrLn "Digite a senha:"
    senha <- getLine
    putStrLn "================================================================================"
    if (Prelude.null email || Prelude.null senha) then do
        putStrLn "Nenhum campo pode estar vazio!"
        desejaContinuar conn (login)
    else do
        userID <- getUserEmailSenha conn email senha
        case userID of
            Just (user_id, tipo) -> do
                if (tipo == "Padrão") then do
                    -- Transição para a tela do cliente após o login
                    menuCliente conn user_id
                else do
                    putStrLn "Administrador" -- Transição para as telas do Administrador
                -- Caso o usuário deslogue:
                menuInicial conn
            Nothing -> do
                putStrLn "Email ou senha incorretos!"
                desejaContinuar conn (login)

getUserEmailSenha::Connection->String->String->IO (Maybe (Int64, String))
getUserEmailSenha conn email senha = do
    user <- query conn "SELECT user_id, user_tipo FROM usuario WHERE user_email = ? AND user_senha = ?" (email, senha)
    return $ listToMaybe user

criarConta::Connection->IO()
criarConta conn = do
    putStrLn "================================================================================"
    putStrLn "                                 CRIAR CONTA                                    "
    putStrLn "================================================================================"
    putStrLn "Preencha seus dados abaixo:\n"
    putStrLn "Nickname:"
    nickname <- getLine
    putStrLn "Nome:"
    nome <- getLine
    putStrLn "E-mail:"
    email <- getLine
    putStrLn "Senha:"
    senha <- getLine
    putStrLn "Confirmar Senha:"
    confirmarSenha <- getLine
    putStrLn "================================================================================"
    if (Prelude.null nickname || Prelude.null nome || Prelude.null email || Prelude.null senha || Prelude.null confirmarSenha) then do
        putStrLn "Nenhum campo pode estar vazio!"
        desejaContinuar conn (criarConta)
    else if (senha /= confirmarSenha) then do
        putStrLn "Senhas digitadas não são iguais!"
        desejaContinuar conn (criarConta)
    else do
        nicknameExistente <- checarNicknameExistente conn nickname
        if (nicknameExistente) then do
            putStrLn "Nickname já exite!"
            desejaContinuar conn (criarConta)
        else do
            emailExistente <- checarEmailExistente conn email
            if (emailExistente) then do
                putStrLn "Email já cadastrado!"
                desejaContinuar conn (criarConta)
            else
                cadastrarConta conn nickname nome email senha


cadastrarConta::Connection->String->String->String->String->IO()
cadastrarConta conn nickname nome email senha = do
    -- Obter a hora atual
    currentTime <- getCurrentTime
    -- Formatar a data atual (YYYY-MM-DD)
    let dataFormatada = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
    let q = "INSERT INTO usuario\
                    \(user_nickname, \
                    \user_nome, \
                    \user_email, \
                    \user_senha, \
                    \user_tipo, \
                    \user_date, \
                    \user_saldo) \
                    \values (?, ?, ?, ?, ?, ?, ?)"
    execute_ conn "BEGIN"
    _ <- execute conn q (nickname, nome, email, senha, "Padrão"::String, dataFormatada, 0::Float)
    execute_ conn "COMMIT"
    putStrLn "Cadastro realizado com sucesso!"
    return()