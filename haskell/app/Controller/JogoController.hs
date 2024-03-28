{-# LANGUAGE OverloadedStrings #-}

module Controller.JogoController where
import Database.PostgreSQL.Simple
import Data.Text (pack, toLower)
import Models.Jogo
import Text.Printf

import Data.Int (Int64)


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
    query_ conn "SELECT * FROM jogo ORDER BY game_data_lancamento" :: IO [Jogo]


getJogosOrdenadosPorNome:: Connection -> IO [Jogo]
getJogosOrdenadosPorNome conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_nome" :: IO [Jogo]


getJogosOrdenadosPorPreco:: Connection -> IO [Jogo]
getJogosOrdenadosPorPreco conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_price" :: IO [Jogo]


getJogosOrdenadosPorAvaliacao:: Connection -> IO [Jogo]
getJogosOrdenadosPorAvaliacao conn = do
    query_ conn "SELECT * FROM jogo ORDER BY game_avaliacao" :: IO [Jogo]


getPrecoDoJogo :: Connection -> Int64 -> IO Double
getPrecoDoJogo conn gameId = do
    [Only preco] <- query conn "SELECT game_price FROM jogo WHERE game_id = ?" (Only gameId)
    return preco


existeJogo :: Connection -> Int64 -> IO Bool
existeJogo conn gameId = do
    [Only count] <- query conn "SELECT COUNT(*) FROM jogo WHERE game_id = ?" (Only gameId)
    return (count > (0 :: Int))