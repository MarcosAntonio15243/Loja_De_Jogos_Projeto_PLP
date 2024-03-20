{-# LANGUAGE OverloadedStrings #-}
module Controller.Admin where
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64)
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
        else cadastraJogo conn nome descricao (getGenero genero) valor

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
