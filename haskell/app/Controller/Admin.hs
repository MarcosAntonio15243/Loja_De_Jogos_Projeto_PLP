{-# LANGUAGE OverloadedStrings #-}
module Controller.Admin where
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64, Int)
import Data.Maybe (listToMaybe)
import Data.Time

menuInicialAdmin::Connection -> IO()
menuInicialAdmin conn = do
    putStrLn "============================================================"
    putStrLn "MENU:"
    putStrLn ""
    putStrLn "1 - Adicionar Jogo"
    putStrLn "2 - Exibir Jogo"
    putStrLn "3 - Atualizar Jogo"
    putStrLn "4 - Remover Jogo"
    putStrLn "5 - Sair"
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Selecione uma opção:  "

    opcao <- getLine

    putStrLn "============================================================"

    case opcao of
        "1" -> do
            putStrLn "Preencha os dados do jogo abaixo:\n"
            adicionarJogo conn
        "2" -> do
            exibirJogo conn
        "3" -> do
            atualizarJogo conn
        "4" -> do
            removeJogo conn
        "5" -> return ()
        _ -> do
            putStrLn "Opção inválida! Por favor, tente novamente."
            menuInicialAdmin conn

adicionarJogo::Connection->IO()
adicionarJogo conn = do
    putStrLn "Digite o nome:"
    nome <- getLine
    putStrLn "Digite a descrição:"
    descricao <- getLine
    putStrLn "1 - Ação e Aventura"
    putStrLn "2 - RPG"
    putStrLn "3 - Terror"
    putStrLn "4 - Estratégia"
    putStrLn "5 - FPS"
    putStrLn "Selecione um gênero:"
    genero <- getLine
    putStrLn "Digite o valor:"
    valor <- readLn :: IO Float
    putStrLn "============================================================"
    if (Prelude.null nome || Prelude.null descricao || validaGenero genero || valor < 0) then do
        putStrLn "Existe algum campo inválido"
        putStrLn "Preencha novamente os dados:"
        adicionarJogo conn
    else do
        jogoExistente <- checarNomeDeJogoExistente conn nome
        if (jogoExistente) then do 
            putStrLn ("Já existe um jogo cadastrado com esse nome")
            adicionarJogo conn
        else do 
            cadastraJogo conn nome descricao (getGenero genero) valor
            menuInicialAdmin conn

getGenero::String -> String
getGenero "1" = "Ação e Aventura"
getGenero "2" = "RPG"
getGenero "3" = "Terror"
getGenero "4" = "Estratégia"
getGenero "5" = "FPS"

validaGenero::String -> Bool
validaGenero x = x /= "1" && x /= "2" && x /= "3" && x /= "4" && x /= "5"

checarNomeDeJogoExistente::Connection->String->IO Bool
checarNomeDeJogoExistente conn nomeDoJogo = do
    [Only count] <- query conn "SELECT COUNT(*) FROM jogo WHERE game_nome = ?" (Only nomeDoJogo)
    return (count /= (0 :: Int))

cadastraJogo::Connection->String->String->String->Float->IO()
cadastraJogo conn nome descricao generos valor = do
    -- Obter a hora atual
    currentTime <- getCurrentTime
    -- Formatar a data atual (YYYY-MM-DD)
    let dataFormatada = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
    let q = "INSERT INTO jogo\
                    \(game_nome, \
                    \game_genero, \
                    \game_description, \
                    \game_data_lancamento, \
                    \game_avaliacao, \
                    \game_price) \
                    \values (?, ?, ?, ?, ?, ?)"
    _ <- execute conn q (nome, generos, descricao, dataFormatada, 0::Int, valor)
    putStrLn "Jogo cadastrado com sucesso"
    return()

exibirJogo::Connection -> IO()
exibirJogo conn = do
    putStrLn "Digite o nome do jogo para exibir as informações sobre ele:"
    nomeJogo <- getLine

    jogoExiste <- checarNomeDeJogoExistente conn nomeJogo

    if jogoExiste then do
        putStrLn ("Aqui estão as informações do jogo " ++ nomeJogo ++ ":")
        infoJogo <- obterInformacoesJogo conn nomeJogo
        mapM_ (mostraInformacoesJogo) infoJogo
        menuInicialAdmin conn

    else do 
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

mostraInformacoesJogo :: (String, String, String, Day, Int, Double) -> IO ()
mostraInformacoesJogo (nome, genero, descricao, dataDeLancamento, avaliacao, preco) = do
    putStrLn $ "Genero: " ++ genero
    putStrLn $ "Descricao: " ++ descricao
    putStrLn $ "Data de lançamento: " ++ formatTime defaultTimeLocale "%Y-%m-%d" dataDeLancamento
    putStrLn $ "Avaliação: " ++ show avaliacao
    putStrLn $ "Preço: R$" ++ show preco
    putStrLn ""

obterInformacoesJogo :: Connection -> String -> IO [(String, String, String, Day, Int, Double)]
obterInformacoesJogo conn nomeJogo = do
    query conn "SELECT game_nome, game_genero, game_description, game_data_lancamento, game_avaliacao, game_price FROM jogo WHERE game_nome = ?" (Only nomeJogo)

atualizarJogo::Connection -> IO()
atualizarJogo conn = do
    putStrLn "Digite o nome do jogo que deseja atualizar os dados: "
    nomeJogo <- getLine

    jogoExiste <- checarNomeDeJogoExistente conn nomeJogo

    if jogoExiste then do
        gameId <- getGameId conn nomeJogo
        atualizaNomeJogo conn gameId
        atualizaDescricaoJogo conn gameId
        atualizaGeneroJogo conn gameId
        atualizaPrecoJogo conn gameId
        menuInicialAdmin conn

    else do
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

atualizaNomeJogo::Connection -> Int64 -> IO()
atualizaNomeJogo conn gameId = do
    putStrLn "Deseja atualizar o nome do jogo? S/N"
    opcao <- getLine
    case opcao of
        "S" -> do
            putStrLn "Digite o novo nome do jogo: "
            novoNomeJogo <- getLine
            jogoExiste <- checarNomeDeJogoExistente conn novoNomeJogo
            if Prelude.null novoNomeJogo then do
                putStrLn "Nome inválido!"
                menuInicialAdmin conn
            else if jogoExiste then do
                putStrLn "Já existe um jogo com esse nome."
                atualizaNomeJogo conn gameId
            else do
                execute conn "UPDATE jogo SET game_nome = ? WHERE game_id = ?" (novoNomeJogo::String, gameId::Int64)  
                putStrLn "Nome de jogo atualizado com sucesso"      
        "N" -> putStr ""
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaNomeJogo conn gameId

atualizaDescricaoJogo::Connection -> Int64 -> IO()
atualizaDescricaoJogo conn gameId = do
    putStrLn "Deseja atualizar a descrição do jogo? S/N"
    opcao <- getLine
    case opcao of
        "S" -> do
            putStrLn "Digite a nova descrição do jogo: "
            novaDescricaoJogo <- getLine
            if Prelude.null novaDescricaoJogo then do
                putStrLn "Descrição inválida!"
                atualizaDescricaoJogo conn gameId
            else do
               execute conn "UPDATE jogo SET game_description = ? WHERE game_id = ?" (novaDescricaoJogo, gameId) 
               putStrLn "Descrição do jogo atualizado com sucesso"    
        "N" -> putStr ""
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaDescricaoJogo conn gameId

atualizaGeneroJogo::Connection -> Int64 -> IO()
atualizaGeneroJogo conn gameId = do
    putStrLn "Deseja atualizar o gênero do jogo? S/N"
    opcao <- getLine
    case opcao of
        "S" -> do
            putStrLn "1 - Ação e Aventura"
            putStrLn "2 - RPG"
            putStrLn "3 - Terror"
            putStrLn "4 - Estratégia"
            putStrLn "5 - FPS"
            putStrLn "Selecione um gênero:"
            opcaoGenero <- getLine
            if validaGenero  opcaoGenero then do
                putStrLn "Gênero inválido!"
                atualizaGeneroJogo conn gameId
            else 
                atualizaGeneroJogoBd conn (getGenero opcaoGenero) gameId  
        "N" -> putStr ""
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaGeneroJogo conn gameId

atualizaGeneroJogoBd::Connection -> String -> Int64 -> IO()
atualizaGeneroJogoBd conn novoGenero gameId = do
    execute conn "UPDATE jogo SET game_genero = ? WHERE game_id = ?" (novoGenero, gameId) 
    putStrLn "Gênero do jogo atualizado com sucesso"

atualizaPrecoJogo::Connection -> Int64 -> IO()
atualizaPrecoJogo conn gameId = do
    putStrLn "Deseja atualizar o preço do jogo? S/N"
    opcao <- getLine
    case opcao of
        "S" -> do
            putStrLn "Digite o novo preço do jogo: "            
            novoPreco <- readLn :: IO Float
            if novoPreco < 0 then do
                putStrLn "Preço inválido!"
                atualizaPrecoJogo conn gameId
            else do
                execute conn "UPDATE jogo SET game_price = ? WHERE game_id = ?" (novoPreco::Float, gameId)     
                putStrLn "Preço do jogo atualizado com sucesso"    
        "N" -> putStr ""
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaPrecoJogo conn gameId

removeJogo::Connection -> IO()
removeJogo conn = do
    putStrLn "Digite o nome do jogo que deseja remover: "
    nomeJogo <- getLine

    jogoExiste <- checarNomeDeJogoExistente conn nomeJogo

    if jogoExiste then do
        putStrLn  $ "Tem certeza que deseja deletar o jogo: " ++ nomeJogo ++ "? S/N"
        confirmacao <- getLine
        case confirmacao of
            "S" -> do
                removeJogoDoSistema conn nomeJogo
                putStrLn "Jogo removido com sucesso!"
                menuInicialAdmin conn
            "N" -> do
                putStrLn  $ "Remoção do jogo " ++ nomeJogo ++ "foi cancelada!"
                menuInicialAdmin conn
            _ -> do
                putStrLn "Opção inválida. Por favor, escolha novamente."
                removeJogo conn

    else do 
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

removeJogoDoSistema :: Connection -> String -> IO ()
removeJogoDoSistema conn nomeJogo = do
    execute conn "DELETE FROM jogo WHERE game_nome = ?" (Only nomeJogo)
    return()

getGameId::Connection -> String -> IO Int64
getGameId conn nomeJogo = do 
    [Only result] <- query conn "SELECT game_id FROM jogo WHERE game_nome = ?" (Only nomeJogo) :: IO [Only Int64]
    return result