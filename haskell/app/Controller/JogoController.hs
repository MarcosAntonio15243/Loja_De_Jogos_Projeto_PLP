{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Controller.JogoController where
import Database.PostgreSQL.Simple
import Data.Text (pack, toLower)
import Models.Jogo
import Text.Printf
import Data.Int (Int64)
import Controller.Util (limparTela)
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Time


printJogos :: [Jogo] -> IO ()
printJogos [] = putStrLn "Nenhum jogo encontrado."
printJogos jogos = do
  putStrLn $ replicate 80 '='
  let titulo = "LISTA DE JOGOS"
      espacosAntes = (80 - length titulo) `div` 2
      espacosDepois = 80 - length titulo - espacosAntes
  putStrLn $ replicate espacosAntes ' ' ++ titulo ++ replicate espacosDepois ' '
  putStrLn $ replicate 80 '='
  mapM_ printJogo jogos
  where
    printJogo jogo = do
      putStrLn $ "ID: " ++ show (game_id jogo)
      putStrLn $ "Nome: " ++ game_nome jogo
      putStrLn $ "Gênero: " ++ game_genero jogo
      putStrLn $ "Preço: " ++ show (game_price jogo)
      putStrLn $ "\ESC[1m\ESC[32mDigite o ID do jogo para ver detalhes\ESC[0m"
      putStrLn $ replicate 80 '-'


printJogoDetalhado :: [Jogo] -> IO ()
printJogoDetalhado [] = putStrLn "Nenhum jogo encontrado."
printJogoDetalhado jogos = do
  putStrLn $ replicate 80 '='
  let titulo = printf "DETALHES DO JOGO [%d]" (game_id (head jogos))
      espacosAntes = (80 - length titulo) `div` 2
      espacosDepois = 80 - length titulo - espacosAntes
  putStrLn $ replicate espacosAntes ' ' ++ titulo ++ replicate espacosDepois ' '
  putStrLn $ replicate 80 '='
  mapM_ printJogoDetalhado jogos
  where
    printJogoDetalhado jogo = do
      putStrLn $ "ID: " ++ show (game_id jogo)
      putStrLn $ "Nome: " ++ game_nome jogo
      putStrLn $ "Gênero: " ++ game_genero jogo
      putStrLn $ "Descrição: " ++ game_description jogo
      putStrLn $ "Data de Lançamento: " ++ show (game_data_lancamento jogo)
      putStrLn $ "Avaliação: " ++ show (game_avaliacao jogo)
      putStrLn $ "Preço: " ++ show (game_price jogo)
      putStrLn $ "\ESC[1m\ESC[32mComprar jogo? [s/n] \ESC[0m"
      putStrLn $ replicate 80 '-'


getJogos:: Connection -> IO [Jogo]
getJogos conn = do
    query_ conn "SELECT * FROM jogo" :: IO [Jogo]


getJogoPorId:: Connection -> Int64 -> IO [Jogo]
getJogoPorId conn id = do
    query conn "SELECT * FROM jogo WHERE game_id = ?" (Only id) :: IO [Jogo]



getJogosPorNome :: Connection -> String -> IO [Jogo]
getJogosPorNome conn nome = do
    query conn "SELECT * FROM jogo WHERE LOWER(game_nome) = LOWER(?)" (Only $ pack nome) :: IO [Jogo]


getJogosAteDeterminadoPreco :: Connection -> Double -> IO [Jogo]
getJogosAteDeterminadoPreco conn preco = do
    query conn "SELECT * FROM jogo WHERE game_price <= ?" (Only preco) :: IO [Jogo]


getJogosPorGenero :: Connection -> String -> IO [Jogo]
getJogosPorGenero conn genero = do
    query conn "SELECT * FROM jogo WHERE LOWER(game_genero) = LOWER(?)" (Only $ pack genero) :: IO [Jogo]


getJogosOrdenadosPorDataLancamento:: Connection -> IO [Jogo]
getJogosOrdenadosPorDataLancamento conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_data_lancamento DESC LIMIT 5" :: IO [Jogo]


getJogosOrdenadosPorNome:: Connection -> IO [Jogo]
getJogosOrdenadosPorNome conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_nome" :: IO [Jogo]


getJogosOrdenadosPorPreco:: Connection -> IO [Jogo]
getJogosOrdenadosPorPreco conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_price" :: IO [Jogo]


getJogosOrdenadosPorAvaliacao :: Connection -> IO [Jogo]
getJogosOrdenadosPorAvaliacao conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_avaliacao DESC LIMIT 5" :: IO [Jogo]


getPrecoDoJogo :: Connection -> Int64 -> IO Double
getPrecoDoJogo conn gameId = do
    [Only preco] <- query conn "SELECT game_price FROM jogo WHERE game_id = ?" (Only gameId)
    return preco


existeJogo :: Connection -> Int64 -> IO Bool
existeJogo conn gameId = do
    [Only count] <- query conn "SELECT COUNT(*) FROM jogo WHERE game_id = ?" (Only gameId)
    return (count > (0 :: Int))

getNomeJogosCliente:: Connection -> Int64 -> IO [String]
getNomeJogosCliente conn userID = do
    result <- query conn "SELECT j.game_nome FROM jogo j INNER JOIN compra c ON j.game_id = c.game_id WHERE c.user_id = ?" (Only userID)
    return $ map fromOnly result

getIDJogosCliente:: Connection -> Int64 -> IO [Int64]
getIDJogosCliente conn userID = do
    result <- query conn "SELECT j.game_id FROM jogo j INNER JOIN compra c ON j.game_id = c.game_id WHERE c.user_id = ?" (Only userID)
    return $ map fromOnly result

getNomeAndIDJogos:: Connection -> Int64 -> IO [(Int64, String)]
getNomeAndIDJogos conn userID = do
    result <- query conn "SELECT j.game_id, j.game_nome FROM jogo j INNER JOIN compra c ON j.game_id = c.game_id WHERE c.user_id = ?" (Only userID)
    return result

getNomeJogoByID :: Connection -> Int64 -> IO String
getNomeJogoByID conn gameID = do
    result <- query conn "SELECT game_nome FROM jogo WHERE game_id = ?" (Only gameID)
    case result of
        [Only nome] -> return nome  
        _ -> return "" 

getFavoritarJogo :: Connection -> Int64 -> Int64 -> IO Bool
getFavoritarJogo conn gameID userID = do
    result <- query conn "SELECT favoritar_jogo FROM compra WHERE user_id = ? and game_id = ?" (userID, gameID) 
    case result of
        [Only favoritado] -> return favoritado
        _ -> error "Nenhum resultado encontrado"

getAvaliacaoAtual :: Connection -> Int64 -> Int64 -> IO Int
getAvaliacaoAtual conn gameID userID = do
    result <- query conn "SELECT avaliacao_compra FROM compra WHERE game_id = ? and user_id = ?" (gameID, userID)
    case result of
        [Only avaliacaoAtual] -> return avaliacaoAtual
        _ -> return (-2)

getJogosFavoritosCliente:: Connection -> Int64 -> IO [Jogo]
getJogosFavoritosCliente conn userID = do
    result <- query conn "SELECT j.game_id, j.game_nome FROM jogo j INNER JOIN compra c ON j.game_id = c.game_id WHERE c.user_id = ? and favoritar_jogo = true" (Only userID)
    return result

registrarAvaliacao :: Connection -> Int64 -> Int64 -> Int -> IO()
registrarAvaliacao conn gameID userID avaliacao = do
    avaliacaoAtual <- getAvaliacaoAtual conn gameID userID

    if avaliacaoAtual == (-1) then do
        _ <- execute conn "UPDATE compra SET avaliacao_compra = ? WHERE game_id = ? and user_id = ?" (avaliacao, gameID, userID)
        putStrLn "============================================================"
        putStrLn "             Avaliação atualizada com sucesso!              "
        putStrLn "============================================================"

    else do
        putStrLn "============================================================"
        putStrLn "\ESC[91mOPS, parece que você ja avaliou este jogo anteriormente\ESC[0m"
        putStrLn $ "Essa é sua avaliação atual: Nota " ++ show avaliacaoAtual
        putStrLn "Deseja alterá-la? (y/n) >"

        opcao <- getLine
        limparTela

        case opcao of
            "y" -> do 
                _ <- execute conn "UPDATE compra SET avaliacao_compra = ? WHERE game_id = ? and user_id = ?" (avaliacao, gameID, userID)
                putStrLn "============================================================"
                putStrLn "             Avaliação atualizada com sucesso!              "
                putStrLn "============================================================"
            "n" -> do
                putStrLn "============================================================"
                putStrLn "           Sua avaliação ao jogo foi cancelada.             "
                putStrLn "============================================================"
            _ -> putStrLn "\ESC[91mOpção inválida!\ESC[0m"
                    
        
registrarComentario :: Connection -> Int64 -> Int64 -> String -> IO()
registrarComentario conn gameID userID comentario = do
    -- Obter a hora atual
    currentTime <- getCurrentTime
    -- Formatar a data atual (YYYY-MM-DD)
    let dataFormatada = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
    let insert = "INSERT INTO comentario \
                        \(id_usuario, \ 
                        \id_jogo, \
                        \comentario_texto, \
                        \comentario_date) \
                        \values (?, ?, ?, ?)" 
    execute_ conn "BEGIN"
    _ <- execute conn insert (userID, gameID, comentario, dataFormatada)
    execute_ conn "COMMIT"
    putStrLn "============================================================"
    putStrLn "            Comentário registrado com sucesso               "
    putStrLn "============================================================"
    return()
    
registrarDenuncia:: Connection -> Int64 -> Int64 -> String-> IO()
registrarDenuncia conn gameID userID motivo = do
    putStrLn "============================================================"
    putStrLn "Explique o motivo da denúncia, tente fornecer o máximo \nde detalhes possíveis."
    putStrLn "\ESC[91mSua denúncia será enviada para o ADMIN, onde será \ndevidamente analisada\ESC[0m"
    putStrLn ""
    putStrLn "Descreva sua denúncia ou \ESC[91mSAIR\ESC[0m para voltar > "

    descricao <- getLine
    limparTela
    
    case descricao of
        "sair" -> return() 
        _   -> do  
            let insert = "INSERT INTO denuncia \
                                \(id_usuario, \ 
                                \id_jogo, \
                                \denuncia_motivo, \
                                \denuncia_descricao) \
                                \values (?, ?, ?, ?)" 
            execute_ conn "BEGIN"
            _ <- execute conn insert (userID, gameID, motivo, descricao)
            execute_ conn "COMMIT"
            putStrLn "============================================================"
            putStrLn "     \ESC[91mDenúncia registrada.\ESC[0m Obrigado pela contribuição       "
            putStrLn "============================================================"
            return()


exibirJogosCliente :: [(Int64, String)] -> IO ()
exibirJogosCliente jogos = do
    let jogosOrdenados = sortBy (\(id1, _) (id2, _) -> compare id1 id2) jogos  -- Ordena os jogos com base nos IDs
    let maxIdWidth = maximum $ map (length . show . fst) jogosOrdenados  -- Calcula a largura máxima do ID
    let formatado = map (\(id, jogo) -> padLeft maxIdWidth ' ' (show id) ++ " | " ++ jogo) jogosOrdenados  -- Formata os jogos com IDs alinhados
    mapM_ putStrLn formatado  -- Imprime os jogos formatados
    putStrLn ""
    putStrLn "\ESC[91mA numeração ao lado do nome do jogo é seu ID\ESC[0m"
    putStrLn "============================================================"

-- Função auxiliar para preencher à esquerda
padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (max 0 (n - length s)) c ++ s

