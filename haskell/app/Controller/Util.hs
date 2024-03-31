{-# LANGUAGE OverloadedStrings #-}

module Controller.Util where
import Database.PostgreSQL.Simple
import Data.Int (Int64)
import Data.Maybe ( listToMaybe, fromMaybe )
import Data.Time (Day)
import Control.Monad (when)

import System.Console.ANSI
import System.Process (callCommand)
import System.Info (os)


limparTela::IO()
limparTela = case os of
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

getEmailByID:: Connection->Int64->IO (Maybe String)
getEmailByID conn id = do
    result <- query conn "SELECT user_email FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only email] -> return $ Just email
        _ -> return Nothing

getTipoByID:: Connection->Int64->IO (Maybe String)
getTipoByID conn id = do
    result <- query conn "SELECT user_tipo FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only tipo] -> return $ Just tipo
        _ -> return Nothing

getDateByID:: Connection->Int64->IO (Maybe Day)
getDateByID conn id = do
    result <- query conn "SELECT user_date FROM usuario WHERE user_id = ?" (Only id) :: IO [Only Day]
    case result of
        [Only date] -> return $ Just date
        _ -> return Nothing

getInformacoesPerfil:: Connection->Int64->IO (String, String, String, String, String)
getInformacoesPerfil conn id = do
    maybeNome <- getNomeByID conn id
    maybeNickname <- getNickByID conn id
    maybeEmail <- getEmailByID conn id
    maybeTipo <- getTipoByID conn id
    maybeDate <- getDateByID conn id

    let nome = fromMaybe "Nome não encontrado" maybeNome
        nickname = fromMaybe "Nickname não encontrado" maybeNickname
        email = fromMaybe "Email não encontrado" maybeEmail
        tipo = fromMaybe "Tipo não encontrado" maybeTipo
        date = maybe "Data não encontrada" show maybeDate -- converte data para String

    return (nome, nickname, email, tipo, date)

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
            limparTela
            putStrLn "================================================================================"
            putStrLn "                        Saldo atualizado com sucesso!                           "
            putStrLn "================================================================================"
        Nothing -> putStrLn "Não foi possível obter o saldo atual do usuário"

getSenhaByID:: Connection -> Int64 -> IO (Maybe String)
getSenhaByID conn id = do
    result <- query conn "SELECT user_senha FROM usuario WHERE user_id = ?" (Only id) :: IO [Only String]
    case result of
        [Only senha] -> return $ Just senha
        _ -> return Nothing

solicitarSenha :: Connection -> Int64 -> IO Bool
solicitarSenha conn userID = do
    putStrLn "================================================================================"
    putStrLn "Confirme sua \ESC[94msenha atual\ESC[0m:"
    senhaDigitada <- getLine

    maybeSenhaReal <- getSenhaByID conn userID
    case maybeSenhaReal of
        Just senhaReal -> do
            if senhaReal == senhaDigitada then
                return True
            else do
                putStrLn "================================================================================"
                putStrLn "\ESC[91mSENHA INCORRETA\ESC[0m"
                putStrLn "================================================================================"
                putStrLn "1. Tentar novamente"
                putStrLn "2. Cancelar"
                putStrLn "================================================================================"
                putStrLn "Selecione uma opção > "

                opcao <- getLine
                limparTela

                case opcao of
                    "1" -> solicitarSenha conn userID
                    "2" -> return False
                    _ -> putStrLn "\ESC[91mOpção inválida!\ESC[0m" >> return False

        Nothing -> putStrLn "ERRO: SENHA NÃO ENCONTRADA" >> return False

alterarNome:: Connection -> Int64 -> IO ()
alterarNome conn id = do
    putStrLn "================================================================================"
    putStrLn "Insira o nome desejado: "
    nomeDesejado <- getLine
    maybeNomeAtual <- getNomeByID conn id

    case maybeNomeAtual of
        Just nomeAtual -> do
            limparTela

            if (Prelude.null nomeDesejado) then
                erroTenteNovamente "OPS, o nome não pode ser vazio." (alterarNome conn id)
            else if nomeAtual == nomeDesejado then
                erroTenteNovamente "OPS, o nome inserido é igual ao nome atual." (alterarNome conn id)

            else do
                _ <- execute conn "UPDATE usuario SET user_nome = ? WHERE user_id = ?" (nomeDesejado, id)
                putStrLn "================================================================================"
                putStrLn "                         Nome atualizado com sucesso!                           "
                putStrLn "================================================================================"

        Nothing -> putStrLn "Nome atual não encontrado"

alterarNick:: Connection -> Int64 -> IO ()
alterarNick conn id = do
    putStrLn "================================================================================"
    putStrLn "Insira o nick desejado: "

    nickDesejado <- getLine
    maybeNickAtual <- getNickByID conn id
    nickExiste <- checarNicknameExistente conn nickDesejado

    case maybeNickAtual of
        Just nickAtual -> do
            limparTela

            if (Prelude.null nickDesejado) then
                erroTenteNovamente "OPS, o nick não pode ser vazio." (alterarNick conn id)
            else if nickAtual == nickDesejado then
                erroTenteNovamente "OPS, o nick inserido é igual ao nick atual." (alterarNick conn id)

            else if nickExiste then
                erroTenteNovamente "OPS, o nick inserido já existe." (alterarNick conn id)

            else do
                _ <- execute conn "UPDATE usuario SET user_nickname = ? WHERE user_id = ?" (nickDesejado, id)
                putStrLn "================================================================================"
                putStrLn "                       Nickname atualizado com sucesso!                         "
                putStrLn "================================================================================"


        Nothing -> putStrLn "Nickname atual não encontrado"

alterarEmail:: Connection -> Int64 -> IO ()
alterarEmail conn id = do
    putStrLn "================================================================================"
    putStrLn "Insira o email desejado: "

    emailDesejado <- getLine
    maybeEmailAtual <- getEmailByID conn id
    emailExiste <- checarEmailExistente conn emailDesejado

    case maybeEmailAtual of
        Just emailAtual -> do
            limparTela

            if (Prelude.null emailDesejado) then
                erroTenteNovamente "OPS, o email não pode ser vazio." (alterarEmail conn id)
            else if emailAtual == emailDesejado then
                erroTenteNovamente "OPS, o email inserido é igual ao email atual." (alterarEmail conn id)

            else if emailExiste then
                erroTenteNovamente "OPS, o email inserido já existe." (alterarEmail conn id)

            else do
                _ <- execute conn "UPDATE usuario SET user_email = ? WHERE user_id = ?" (emailDesejado, id)
                putStrLn "================================================================================"
                putStrLn "                         Email atualizado com sucesso!                          "
                putStrLn "================================================================================"

        Nothing -> putStrLn "Email atual não encontrado"


alterarSenha:: Connection -> Int64 -> IO()
alterarSenha conn id = do
    confirmarSenhaAtual <- solicitarSenha conn id

    Control.Monad.when confirmarSenhaAtual $ do -- Se der True vai executar o bloco "do", caso contrário apenas retorna
        putStrLn "================================================================================"
        putStrLn "Insira a \ESC[94mnova senha\ESC[0m:" 

        novaSenha <- getLine
        maybeSenhaAtual <- getSenhaByID conn id
        let senhaAtual = fromMaybe "Senha não encontrada" maybeSenhaAtual

        if (Prelude.null novaSenha) then
            erroTenteNovamente "OPS, a senha não pode ser vazia." (alterarSenha conn id)
        else if senhaAtual == novaSenha then
            erroTenteNovamente "Ops, a nova senha é igual a senha atual" (alterarSenha conn id)

        else do
            limparTela
            _ <- execute conn "UPDATE usuario SET user_senha = ? WHERE user_id = ?" (novaSenha, id)
            putStrLn "================================================================================"
            putStrLn "                        Senha atualizada com sucesso!                           "
            putStrLn "================================================================================"


erroTenteNovamente:: String -> IO() -> IO()
erroTenteNovamente message acao1 = do
    putStrLn "================================================================================"
    putStrLn $ "\ESC[91m"++ message ++"\ESC[0m"
    putStrLn "================================================================================"
    putStrLn "1. Tentar novamente"
    putStrLn "2. Cancelar"
    putStrLn "================================================================================"
    putStrLn "Selecione uma opção > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> acao1
        "2" -> return ()
        _ -> putStrLn "\ESC[91mOpção inválida!\ESC[0m"

deletaConta:: Connection -> Int64 -> IO ()
deletaConta conn userID = do
    _ <- execute conn "DELETE FROM compra WHERE user_id = ?" (Only userID) -- deleta todas as compras do usuario
    _ <- execute conn "DELETE FROM comentario WHERE id_usuario = ?" (Only userID) -- deleta todos os comentarios
    _ <- execute conn "DELETE FROM denuncia WHERE id_usuario = ?" (Only userID) -- deleta todas as denuncias
    _ <- execute conn "DELETE FROM mensagem WHERE id_remetente = ?" (Only userID) -- deleta todas as mensagens que o usuario enviou
    _ <- execute conn "DELETE FROM usuario WHERE user_id = ?" (Only userID) -- por fim, deleta o usuario
    limparTela
    putStrLn "================================================================================"
    putStrLn "                          Sua conta foi deletada!                               "
    putStrLn "================================================================================"
    return ()