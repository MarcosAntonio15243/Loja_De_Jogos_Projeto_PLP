{-# LANGUAGE OverloadedStrings #-}
module Controller.Admin where
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64)
import Data.Maybe (listToMaybe)

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
    putStr "Selecione uma opção:  "

    opcao <- getLine

    putStrLn "============================================================"

    case opcao of
        "1" -> do
            putStrLn "Preencha os dados do jogo abaixo:\n"
            adicionarJogo conn
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
    putStrLn "Escreva o gênero:"
    generos <- getLine
    putStrLn "Digite o valor:"
    valor <- readLn :: IO Float
    putStrLn "============================================================"
    if (Prelude.null nome || Prelude.null descricao || Prelude.null generos || valor < 0) then do
        putStrLn "Existe algum campo inválido"
        putStrLn "Preencha novamente os dados:"
        adicionarJogo conn
    else do
        jogoExistente <- checarNomeDeJogoExistente conn nome
        if (jogoExistente) then do 
            putStrLn ("Já existe um jogo cadastrado com esse nome")
            adicionarJogo conn
        else cadastraJogo conn nome descricao generos valor


--getGeneros :: String -> String
--getGeneros str = do
 --  opcao <- getLine
 ---   putStrLn "============================================================"
  --  putStrLn "GÊNEROS:"
  --  putStrLn ""
 --  putStrLn "1 - Ação"
  --  putStrLn "2 - Aventura"
  --  putStrLn "3 - RPG"
  --  putStrLn "4 - Terror"
  --  putStrLn "5 - Estratégia"
  --  putStrLn "6 - FPS"
 ---   putStrLn "7 - Finalizar"
  --  putStrLn ""
  ----  putStrLn "============================================================"
  --  putStr "Selecione uma opção:  "
  --  case opcao of
   --         "1" -> getGeneros (str ++ "Ação ")
  --          "2" -> getGeneros (str ++ "Aventura ")
  --          "3" -> getGeneros (str ++ "RPG  ")
 --           "4" -> getGeneros (str ++ "Terror ")
  --          "5" -> getGeneros (str ++ "Estratégia ")
--"6" -> getGeneros (str ++ "FPS ")
 --           "7" -> str
 --           _ -> do
--                putStrLn "Opção inválida! Por favor, tente novamente."
--                getGeneros str

checarNomeDeJogoExistente::Connection->String->IO Bool
checarNomeDeJogoExistente conn nomeDoJogo = do
    [Only count] <- query conn "SELECT COUNT(*) FROM game WHERE game_nome = ?" (Only nomeDoJogo)
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