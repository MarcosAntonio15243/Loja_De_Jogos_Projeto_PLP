{-# LANGUAGE OverloadedStrings #-}
module Controller.User where
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Controller.Admin

menuInicial::Connection -> IO()
menuInicial conn = do
    putStrLn "============================================================"
    putStrLn "MENU:"
    putStrLn ""
    putStrLn "1 - Login"
    putStrLn "2 - Criar uma conta"
    putStrLn "3 - Sair"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione uma opção:  "

    opcao <- getLine

    putStrLn "============================================================"

    case opcao of
        "1" -> do
            putStrLn "Preencha seus dados abaixo:"
            login conn
        "2" -> do
            putStrLn "Preencha seus dados abaixo:"
            criarConta conn
        "3" -> return ()
        _ -> do
            putStrLn "Opção inválida! Por favor, tente novamente."
            menuInicial conn

login::Connection->IO()
login conn = do
    putStrLn "Digite o e-mail:"
    email <- getLine
    putStrLn "Digite a senha:"
    senha <- getLine
    putStrLn "============================================================"
    if (Prelude.null email || Prelude.null senha) then do
        putStrLn "Nenhum campo pode estar vazio!"
        putStrLn "Preencha novamente os seus dados:"
        login conn
    else do
        userID <- getUserEmailSenha conn email senha
        case userID of
            Just (id, tipo) -> do
                if (tipo == "Padrão") then do
                    putStrLn "Padrão"
                    --tela cliente
                else do
                    putStrLn "Not padrão"
                    menuInicialAdmin conn
            Nothing -> do
                putStrLn "Email ou senha incorretos! Tente novamente.\n"
                login conn

getUserEmailSenha::Connection->String->String->IO (Maybe (Int64, String))
getUserEmailSenha conn email senha = do
    user <- query conn "SELECT user_id, user_tipo FROM usuario WHERE user_email = ? AND user_senha = ?" (email, senha)
    return $ listToMaybe user


criarConta::Connection->IO()
criarConta conn = do
    putStrLn "Nome:"
    nome <- getLine
    putStrLn "E-mail:"
    email <- getLine
    putStrLn "Senha:"
    senha <- getLine
    putStrLn "Confirmar Senha:"
    confirmarSenha <- getLine
    putStrLn "============================================================"
    if (Prelude.null nome || Prelude.null email || Prelude.null senha || Prelude.null confirmarSenha) then do
        putStrLn "Nenhum campo pode estar vazio!"
        putStrLn "Preencha novamente os seus dados:"
        criarConta conn
    else if (senha /= confirmarSenha) then do
        putStrLn "Senhas digitadas não são iguais!"
        putStrLn "Preencha novamente os seus dados:"
        criarConta conn
    else do
        emailExistente <- checarEmailExistente conn email
        if (emailExistente) then do
            putStrLn "Email já cadastrado!"
            putStrLn "Preencha novamente utilizando outro endereço de email."
            criarConta conn
        else
            cadastrarConta conn nome email senha
    menuInicial conn

checarEmailExistente::Connection->String->IO Bool
checarEmailExistente conn email = do
    [Only count] <- query conn "SELECT COUNT(*) FROM usuario WHERE user_email = ?" (Only email)
    return (count /= (0 :: Int))

cadastrarConta::Connection->String->String->String->IO()
cadastrarConta conn nome email senha = do
    -- Obter a hora atual
    currentTime <- getCurrentTime
    -- Formatar a data atual (YYYY-MM-DD)
    let dataFormatada = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
    let q = "INSERT INTO usuario\
                    \(user_nome, \
                    \user_email, \
                    \user_senha, \
                    \user_tipo, \
                    \user_date, \
                    \user_saldo) \
                    \values (?, ?, ?, ?, ?, ?)"
    execute_ conn "BEGIN"
    _ <- execute conn q (nome, email, senha, "Padrão"::String, dataFormatada, 0::Float)
    execute_ conn "COMMIT"
    putStrLn "Cadastro realizado com sucesso!"
    return()