{-# LANGUAGE OverloadedStrings #-}

module Controller.Cliente where
import Database.PostgreSQL.Simple
import LocalDB.ConnectionDB
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Maybe (fromMaybe)
import Controller.Util
import Controller.JogoController
import Text.Read (readMaybe)
import Data.Char

import Controller.CompraController
import Text.Printf

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
                "1" -> do menuJogos conn user_id
                "2" -> do mensagens conn user_id
                "3" -> do perfilCliente conn user_id
                "4" -> do
                    return ()
                _ -> do
                    putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
                    menuCliente conn user_id
        Nothing -> do putStrLn "Id usuário Inválido!"

menuJogos::Connection->Int64->IO()
menuJogos conn user_id = do

    jogos <- getJogos conn
    printJogos jogos

    putStrLn "Digite um id (Ou tecle ENTER para sair): "
    idJogo <- getLine
    
    if (Prelude.null idJogo) then do
        limparTela
        menuCliente conn user_id
    else do
        let jogoId = read idJogo :: Int64
        jogo <- getJogoPorId conn jogoId 
        limparTela
        if (jogo == []) then do
            printJogoDetalhado jogo
            desejaContinuar conn user_id (menuJogos)
        else do
            printJogoDetalhado jogo
            opcao <- getLine
            if (opcao == "s" || opcao == "S") then do
                realizaCompra conn user_id (jogoId)
                menuCliente conn user_id
            else do
                limparTela
                menuJogos conn user_id

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
exibeMensagens _ _ [] = return ()
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
    return ()


perfilCliente:: Connection -> Int64 -> IO()
perfilCliente conn userID = do

    maybeNickname <- getNickByID conn userID
    case maybeNickname of
        Just nickname -> do
            putStrLn "============================================================"
            putStrLn ("\ESC[94m" ++ nickname ++ "\ESC[0m" ++ "'s Perfil      ")
            putStrLn "============================================================"
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
                    jogosCliente conn userID
                "2" -> do
                    carteiraCliente conn userID
                "3" -> do
                    editarPerfil conn userID
                "4" -> do
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
    putStrLn $ "       Saldo Disponível: R$" ++ printf "%.2f" (fromJust saldo)
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


selecionarSaldo :: Connection -> Int64 -> Double -> IO ()
selecionarSaldo conn userID saldo = do
    confirmarSenha <- solicitarSenha conn userID

    if confirmarSenha
        then do addSaldoByID conn userID saldo
                carteiraCliente conn userID
    else do
        selecionarQntSaldo conn userID


editarPerfil:: Connection -> Int64 -> IO ()
editarPerfil conn userID = do
    (nome, nick, email, tipo, date) <- getInformacoesPerfil conn userID

    putStrLn "============================================================"
    putStrLn "               Informações do seu Perfil:                   "
    putStrLn "============================================================"
    putStrLn ""
    putStrLn $ "Nome: " ++ nome
    putStrLn $ "Nickname: " ++ nick
    putStrLn $ "Email: " ++ email
    putStrLn   "Senha: *******"
    putStrLn $ "Tipo de usuário: " ++ tipo ++ " \ESC[91m- Não alterável.\ESC[0m"
    putStrLn $ "Data de criação: " ++ date ++ " \ESC[91m- Não alterável.\ESC[0m"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "1. Alterar Nome"
    putStrLn "2. Alterar Nickname"
    putStrLn "3. Alterar Email"
    putStrLn "4. Alterar Senha"
    putStrLn "5. \ESC[91mExcluir conta\ESC[0m"
    putStrLn "6. Voltar"
    putStrLn "============================================================"
    putStrLn "Selecione uma opção > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> do
            alterarNome conn userID
            eP
        "2" -> do
            alterarNick conn userID
            eP
        "3" -> do
            alterarEmail conn userID
            eP
        "4" -> do
            alterarSenha conn userID
            eP
        "5" -> excluirConta conn userID
        "6" -> perfilCliente conn userID
        _ -> do
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            eP
        where eP = editarPerfil conn userID


excluirConta :: Connection -> Int64 -> IO ()
excluirConta conn userID = do
    putStrLn "============================================================"
    putStrLn "\ESC[91m--------------------------Atenção---------------------------\ESC[0m"
    putStrLn "\ESC[91mExcluir sua conta irá deletar todas suas mensagens, \njogos, e qualquer outra informação \nligada a sua conta. Esta ação não pode ser revertida!\ESC[0m"
    putStrLn ""
    putStrLn "Tem certeza que deseja continuar? (y/n) >"

    opcao <- getLine
    limparTela

    let lowerInput = map toLower opcao  -- Convertendo para minúsculas
    case lowerInput of
        "y" -> do 
            confirmaSenha <- solicitarSenha conn userID
            if confirmaSenha then
                deletaConta conn userID
            else
                editarPerfil conn userID
        "n" -> editarPerfil conn userID

        _ -> do 
            putStrLn "\ESC[91mOpção inválida!\ESC[0m"
            excluirConta conn userID

jogosCliente:: Connection -> Int64 -> IO()
jogosCliente conn userID = do
    result <- getNomeAndIDJogos conn userID -- retorna uma lista com [(id, nomeJogo)] de todos os jogos do usuario
    putStrLn "============================================================"
    putStrLn "                       Meus Jogos                           "
    putStrLn "============================================================"
    exibirJogosCliente result

    putStrLn "Digite \ESC[91mSAIR\ESC[0m para voltar"
    putStrLn "Insira o ID do jogo que deseja acessar > "

    input <- getLine
    limparTela
    
    let lowerInput = map toLower input  -- Convertendo para minúsculas
    case lowerInput of
        "sair" -> perfilCliente conn userID
        _   -> do
            existeIDJogoCliente conn userID input -- verifica se o id passado é de algum jogo do usuario

    
acessarJogo:: Connection -> Int64 -> Int64 -> IO()
acessarJogo conn userID gameID = do

    estaFavoritado <- getFavoritarJogo conn gameID userID
    nomeJogo <- getNomeJogoByID conn gameID
    let estrela = if estaFavoritado then "\x1b[33m*\x1b[0m" else ""

    putStrLn "============================================================"
    putStrLn $ nomeJogo ++ " " ++ estrela
    putStrLn "============================================================"
    putStrLn ""
    putStrLn "1. Avaliar jogo"
    putStrLn "2. Favoritar jogo"
    putStrLn "3. Deixar um comentário"
    putStrLn "4. Denunciar jogo"
    putStrLn "5. Voltar"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione uma opção > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> do 
            avaliarJogo conn gameID userID 
            aJ
        "2" -> do
            favoritarJogo conn gameID userID
            aJ
        "3" -> do
            comentarJogo conn gameID userID
            aJ
        "4" -> do 
            denunciarJogo conn gameID userID
            aJ    
        "5" -> jogosCliente conn userID

        _ -> do
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            aJ
        where aJ = acessarJogo conn userID gameID 

denunciarJogo :: Connection -> Int64 -> Int64 -> IO ()
denunciarJogo conn gameID userID = do
    putStrLn "============================================================"
    putStrLn "                     \ESC[91mDenúncia\ESC[0m              "
    putStrLn "============================================================"
    putStrLn ""
    putStrLn "1. Não funciona/possui algum problema crítico"
    putStrLn "2. Contém vírus/malwares"
    putStrLn "3. Viola leis judiciais"
    putStrLn "4. Conteúdo adulto não classificado"
    putStrLn "5. Fraude (tenta obter dados de forma fraudulenta)"
    putStrLn "6. Outros"
    putStrLn "7. Voltar"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione o motivo da denúncia > "

    opcao <- getLine
    limparTela

    case opcao of
        "1" -> do 
            registrarDenuncia conn gameID userID "Não funciona/possui algum problema crítico"
            dJ
        "2" -> do
            registrarDenuncia conn gameID userID "Contém vírus/malware"
            dJ
        "3" -> do
            registrarDenuncia conn gameID userID "Viola leis judiciais"
            dJ
        "4" -> do 
            registrarDenuncia conn gameID userID "Conteúdo adulto não classificado"
            dJ
        "5" -> do
            registrarDenuncia conn gameID userID "Fraude"
            dJ
        "6" -> do
            registrarDenuncia conn gameID userID "Outros"
            dJ
        "7" -> return() --acessarJogo conn userID gameID

        _ -> do
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            dJ

        where dJ = denunciarJogo conn userID gameID

existeIDJogoCliente :: Connection -> Int64 -> String  -> IO ()
existeIDJogoCliente conn userID input = do

    let maybeOpcao = readMaybe input :: Maybe Int64
    case maybeOpcao of
        Just opcao -> do
            iDsJogosCliente <- getIDJogosCliente conn userID
            
            if opcao `elem` iDsJogosCliente then do
                acessarJogo conn userID opcao 

            else do 
                erroTenteNovamente "ID não existe" (jogosCliente conn userID)
                jogosCliente conn userID

        Nothing -> do 
            putStrLn "Entrada inválida. Por favor, digite um número."
            jogosCliente conn userID

avaliarJogo:: Connection -> Int64 -> Int64 -> IO()
avaliarJogo conn gameID userID = do

    putStrLn "============================================================"
    putStrLn "Insira uma nota de 0 a 10 > "

    nota <- getLine

    let maybeNota = readMaybe nota :: Maybe Int
    case maybeNota of
        Just nota -> do
            if nota < 0 || nota > 10 then
                erroTenteNovamente "Por favor, insira um valor dentro da faixa" (avaliarJogo conn gameID userID)
            else do
                registrarAvaliacao conn gameID userID nota

        Nothing -> putStrLn "Entrada inválida. Por favor, digite um número."

favoritarJogo :: Connection -> Int64 -> Int64 -> IO ()
favoritarJogo conn gameID userID = do
    estaFavoritado <- getFavoritarJogo conn gameID userID

    if estaFavoritado then do
        putStrLn "============================================================"
        putStrLn "\ESC[91mO jogo já está em seus favoritos\ESC[0m"
        putStrLn "Deseja removê-lo dos favoritos? (y/n) >"

        opcao <- getLine
        limparTela

        let lowerInput = map toLower opcao  -- Convertendo para minúsculas
        case lowerInput of
            "y" -> do 
                _ <- execute conn "UPDATE compra SET favoritar_jogo = false WHERE game_id = ? and user_id = ?" (gameID, userID)
                putStrLn "============================================================"
                putStrLn "                Jogo removido dos favoritos!                "
                putStrLn "============================================================"
            "n" -> do
                return()
            _ -> do
                putStrLn "\ESC[91mOpção inválida!\ESC[0m"

    else do
        _ <- execute conn "UPDATE compra SET favoritar_jogo = true WHERE game_id = ? and user_id = ?" (gameID, userID)
        putStrLn "============================================================"
        putStrLn "                Jogo favoritado com sucesso                 "
        putStrLn "============================================================"


comentarJogo :: Connection -> Int64 -> Int64 -> IO()
comentarJogo conn gameID userID = do
    putStrLn "============================================================"
    putStrLn "Insira seu comentário ou \ESC[91mSAIR\ESC[0m para voltar > "

    comentario <- getLine
    limparTela
    
    let lowerInput = map toLower comentario  -- Convertendo para minúsculas
    case lowerInput of
        "sair" -> return() --acessarJogo conn userID gameID 
        _   -> do
            registrarComentario conn gameID userID comentario





