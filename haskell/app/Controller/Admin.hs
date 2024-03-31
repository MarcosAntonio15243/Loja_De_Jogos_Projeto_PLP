{-# LANGUAGE OverloadedStrings #-}
module Controller.Admin where
import Database.PostgreSQL.Simple
import Data.Time.Format
import Data.Time.Clock
import Data.Int (Int64, Int)
import Data.Maybe (listToMaybe)
import Data.Time
import Controller.Util
import Models.Denuncia
import Data.Char (toUpper)
import Controller.JogoController
import Data.Text.Encoding (encodeUtf32BE)

menuInicialAdmin::Connection -> IO()
menuInicialAdmin conn = do
    putStrLn "================================================================================"
    putStrLn "                             HOME - ADMINISTRADOR                               "
    putStrLn "================================================================================"
    putStrLn "MENU:"
    putStrLn ""
    putStrLn "1 - Adicionar Jogo"
    putStrLn "2 - Exibir Jogo"
    putStrLn "3 - Atualizar Jogo"
    putStrLn "4 - Remover Jogo"
    putStrLn "5 - Exibir denuncia"
    putStrLn "6 - Sair"
    putStrLn ""
    putStrLn "================================================================================"
    putStrLn "Selecione uma opção:  "

    opcao <- getLine

    putStrLn "================================================================================"

    case opcao of
        "1" -> do
            limparTela
            adicionarJogo conn
        "2" -> do
            limparTela
            exibirJogo conn
        "3" -> do
            limparTela
            atualizarJogo conn
        "4" -> do
            limparTela
            removeJogo conn
        "5" -> do
            limparTela
            exibeDenuncia conn
        "6" -> do
            limparTela
            return()
        _ -> do
            limparTela
            putStrLn "\ESC[91mOpção inválida! Por favor, tente novamente.\ESC[0m"
            menuInicialAdmin conn

adicionarJogo::Connection->IO()
adicionarJogo conn = do
    putStrLn "================================================================================"
    putStrLn "                             ADICIONAR NOVO JOGO                                "
    putStrLn "================================================================================"
    putStrLn "Digite os dados do jogo abaxo:"
    putStrLn "================================================================================"
    putStrLn "Digite o nome:"
    nome <- getLine
    putStrLn "================================================================================"
    putStrLn "Digite a descrição:"
    descricao <- getLine
    putStrLn "================================================================================"
    putStrLn "Selecione um gênero:"
    putStrLn "1 - Ação e Aventura"
    putStrLn "2 - RPG"
    putStrLn "3 - Terror"
    putStrLn "4 - Estratégia"
    putStrLn "5 - FPS"
    genero <- getLine
    putStrLn "================================================================================"
    putStrLn "Digite o valor:"
    valor <- readLn :: IO Float
    putStrLn "================================================================================"
    if (Prelude.null nome || Prelude.null descricao || validaGenero genero || valor < 0) then do
        limparTela
        putStrLn "Existe algum campo inválido"
        putStrLn "Preencha novamente os dados:"
        adicionarJogo conn
    else do
        jogoExistente <- checarNomeDeJogoExistente conn nome
        if (jogoExistente) then do 
            limparTela
            putStrLn ("Já existe um jogo cadastrado com esse nome")
            adicionarJogo conn
        else do
            limparTela
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
    _ <- execute conn q (nome, generos, descricao, dataFormatada, 0.0::Float, valor)
    putStrLn "Jogo cadastrado com sucesso"
    return()

exibirJogo::Connection -> IO()
exibirJogo conn = do
    putStrLn "================================================================================"
    putStrLn "Digite o nome do jogo para exibir as informações sobre ele:"
    nomeJogo <- getLine

    jogoExiste <- checarNomeDeJogoExistente conn nomeJogo
    limparTela

    if jogoExiste then do
        putStrLn ("Aqui estão as informações do jogo " ++ nomeJogo ++ ":")
        infoJogo <- obterInformacoesJogo conn nomeJogo
        mapM_ (mostraInformacoesJogo) infoJogo
        putStrLn ""
        putStrLn ("(Pressione qualquer tecla para voltar ao menu inicial)")
        opcao <- getLine
        limparTela
        menuInicialAdmin conn

    else do 
        limparTela
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

mostraInformacoesJogo :: (String, String, String, Day, Double, Double) -> IO ()
mostraInformacoesJogo (nome, genero, descricao, dataDeLancamento, avaliacao, preco) = do
    putStrLn "================================================================================"
    putStrLn $ "Gênero: " ++ genero
    putStrLn $ "Descrição: " ++ descricao
    putStrLn $ "Data de lançamento: " ++ formatTime defaultTimeLocale "%Y-%m-%d" dataDeLancamento
    putStrLn $ "Avaliação: " ++ show avaliacao
    putStrLn $ "Preço: R$" ++ show preco


obterInformacoesJogo :: Connection -> String -> IO [(String, String, String, Day, Double, Double)]
obterInformacoesJogo conn nomeJogo = do
    query conn "SELECT game_nome, game_genero, game_description, game_data_lancamento, game_avaliacao, game_price FROM jogo WHERE game_nome = ?" (Only nomeJogo)

atualizarJogo::Connection -> IO()
atualizarJogo conn = do
    putStrLn "Jogos cadastrados no sistema: "
    putStrLn ""
    result <- getNomeAndIDTodosJogos conn
    exibirJogosCliente result
    putStrLn "Digite o id do jogo que deseja atualizar os dados: "
    idGame <- getLine
    let gameId = read idGame :: Int64
    jogoExiste <- existeJogo conn gameId

    if jogoExiste then do
        atualizaNomeJogo conn gameId
        atualizaDescricaoJogo conn gameId
        atualizaGeneroJogo conn gameId
        atualizaPrecoJogo conn gameId
        menuInicialAdmin conn

    else do
        limparTela
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

atualizaNomeJogo::Connection -> Int64 -> IO()
atualizaNomeJogo conn gameId = do
    putStrLn "================================================================================"
    putStrLn "Deseja atualizar o nome do jogo? S/N"
    opcao <- getLine
    let opcaoMaiuscula = map toUpper opcao
    case opcaoMaiuscula of
        "S" -> do
            putStrLn "================================================================================"
            putStrLn "Digite o novo nome do jogo: "
            novoNomeJogo <- getLine
            jogoExiste <- checarNomeDeJogoExistente conn novoNomeJogo
            if Prelude.null novoNomeJogo then do
                limparTela
                putStrLn "Nome inválido!"
                menuInicialAdmin conn
            else if jogoExiste then do
                limparTela
                putStrLn "Já existe um jogo com esse nome."
                atualizaNomeJogo conn gameId
            else do
                limparTela
                putStrLn "================================================================================"
                execute conn "UPDATE jogo SET game_nome = ? WHERE game_id = ?" (novoNomeJogo::String, gameId::Int64)  
                putStrLn "Nome de jogo atualizado com sucesso"      
        "N" -> putStr ""
        _ -> do
            putStrLn "Opção inválida. Por favor, escolha novamente."
            limparTela
            atualizaNomeJogo conn gameId

atualizaDescricaoJogo::Connection -> Int64 -> IO()
atualizaDescricaoJogo conn gameId = do
    putStrLn "================================================================================"
    putStrLn "Deseja atualizar a descrição do jogo? S/N"
    opcao <- getLine
    let opcaoMaiuscula = map toUpper opcao
    case opcaoMaiuscula of
        "S" -> do
            putStrLn "================================================================================"
            putStrLn "Digite a nova descrição do jogo: "
            novaDescricaoJogo <- getLine
            if Prelude.null novaDescricaoJogo then do
                limparTela
                putStrLn "Descrição inválida!"
                atualizaDescricaoJogo conn gameId
            else do
               execute conn "UPDATE jogo SET game_description = ? WHERE game_id = ?" (novaDescricaoJogo, gameId) 
               putStrLn "================================================================================"
               putStrLn "Descrição do jogo atualizado com sucesso"    
        "N" -> putStr ""
        _ -> do
            limparTela
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaDescricaoJogo conn gameId

atualizaGeneroJogo::Connection -> Int64 -> IO()
atualizaGeneroJogo conn gameId = do
    putStrLn "================================================================================"
    putStrLn "Deseja atualizar o gênero do jogo? S/N"
    opcao <- getLine
    let opcaoMaiuscula = map toUpper opcao
    case opcaoMaiuscula of
        "S" -> do
            putStrLn "================================================================================"
            putStrLn "Selecione um gênero:"
            putStrLn "1 - Ação e Aventura"
            putStrLn "2 - RPG"
            putStrLn "3 - Terror"
            putStrLn "4 - Estratégia"
            putStrLn "5 - FPS"
            opcaoGenero <- getLine
            if validaGenero opcaoGenero then do
                limparTela
                putStrLn "Gênero inválido!"
                atualizaGeneroJogo conn gameId
            else 
                atualizaGeneroJogoBd conn (getGenero opcaoGenero) gameId  
        "N" -> putStr ""
        _ -> do
            limparTela
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaGeneroJogo conn gameId

atualizaGeneroJogoBd::Connection -> String -> Int64 -> IO()
atualizaGeneroJogoBd conn novoGenero gameId = do
    execute conn "UPDATE jogo SET game_genero = ? WHERE game_id = ?" (novoGenero, gameId) 
    putStrLn "================================================================================"
    putStrLn "Gênero do jogo atualizado com sucesso"

atualizaPrecoJogo::Connection -> Int64 -> IO()
atualizaPrecoJogo conn gameId = do
    putStrLn "================================================================================"
    putStrLn "Deseja atualizar o preço do jogo? S/N"
    opcao <- getLine
    let opcaoMaiuscula = map toUpper opcao
    case opcaoMaiuscula of
        "S" -> do
            putStrLn "================================================================================"
            putStrLn "Digite o novo preço do jogo: "            
            novoPreco <- readLn :: IO Float
            if novoPreco < 0 then do
                limparTela
                putStrLn "Preço inválido!"
                atualizaPrecoJogo conn gameId
            else do
                limparTela
                execute conn "UPDATE jogo SET game_price = ? WHERE game_id = ?" (novoPreco::Float, gameId)     
                putStrLn "================================================================================"
                putStrLn "Preço do jogo atualizado com sucesso"    
        "N" -> putStr ""
        _ -> do
            limparTela
            putStrLn "Opção inválida. Por favor, escolha novamente."
            atualizaPrecoJogo conn gameId

removeJogo::Connection -> IO()
removeJogo conn = do
    putStrLn "================================================================================"
    putStrLn "Digite o nome do jogo que deseja remover: "
    nomeJogo <- getLine

    jogoExiste <- checarNomeDeJogoExistente conn nomeJogo

    if jogoExiste then do
        putStrLn "================================================================================"
        putStrLn  $ "Tem certeza que deseja deletar o jogo: " ++ nomeJogo ++ "? S/N"
        confirmacao <- getLine
        let confirmacaoMaiuscula = map toUpper confirmacao
        case confirmacaoMaiuscula of
            "S" -> do
                limparTela
                putStrLn "================================================================================"
                removeJogoDoSistema conn nomeJogo
                putStrLn "Jogo removido com sucesso!"
                menuInicialAdmin conn
            "N" -> do
                limparTela
                putStrLn "================================================================================"
                putStrLn  $ "Remoção do jogo " ++ nomeJogo ++ "foi cancelada!"
                menuInicialAdmin conn
            _ -> do
                limparTela
                putStrLn "================================================================================"
                putStrLn "Opção inválida. Por favor, escolha novamente."
                removeJogo conn

    else do 
        limparTela
        putStrLn "================================================================================"
        putStrLn "Não existe um jogo com esse nome"
        menuInicialAdmin conn

removeJogoDoSistema :: Connection -> String -> IO ()
removeJogoDoSistema conn nomeJogo = do
    execute conn "DELETE FROM jogo WHERE game_nome = ?" (Only nomeJogo)
    return()

exibeDenuncia :: Connection -> IO()
exibeDenuncia conn = do
    putStrLn "================================================================================"
    infoDenuncia <- getDenuncia conn
    if infoDenuncia == [] then do
         limparTela
         putStrLn "================================================================================"
         putStrLn "Não existem denuncias no momento!"
         menuInicialAdmin conn
    else do
         putStrLn "Aqui estao as informações sobre a denuncia"
         exibeInformacoesDenuncia conn (head infoDenuncia)
    putStrLn "A denuncia é válida? S/N"
    opcao <- getLine
    let opcaoMaiuscula = map toUpper opcao
    case opcaoMaiuscula of
        "S" -> do
            limparTela
            nomeJogo <- getNomeJogoId conn (game_id (head infoDenuncia)) 
            removeJogoDoSistema conn nomeJogo
            apagaDenuncia conn (denuncia_id (head infoDenuncia))
            putStrLn "================================================================================"
            putStrLn "Como a denuncia é válida, o jogo foi removido!"
            menuInicialAdmin conn
        "N" -> do
            limparTela
            apagaDenuncia conn (denuncia_id (head infoDenuncia))
            putStrLn "================================================================================"
            putStrLn "Denuncia invalida!"
            menuInicialAdmin conn
        _ -> do
            limparTela
            putStrLn "================================================================================"
            putStrLn "Opção inválida"
            exibeDenuncia conn

exibeInformacoesDenuncia :: Connection -> Denuncia -> IO()
exibeInformacoesDenuncia conn denuncia = do
     showDenuncia denuncia
     where
        showDenuncia d = do
            nomeUsuario <- getNomeUsuarioId conn (user_id d)
            nomeJogo <- getNomeJogoId conn (game_id d)
            putStrLn "================================================================================"
            putStrLn $ "Id da denuncia: " ++ show (denuncia_id d)
            putStrLn $ "Nome do usuario: " ++  nomeUsuario
            putStrLn $ "Nome do jogo: " ++  nomeJogo
            putStrLn $ "Motivo da denuncia: " ++ (denuncia_motivo d)
            putStrLn $ "Descricao da denuncia: " ++ (denuncia_descricao d)
            putStrLn $ "Dia da denuncia: " ++ formatTime defaultTimeLocale "%Y-%m-%d" (denuncia_data d)
            putStrLn "================================================================================"

getDenuncia :: Connection -> IO [Denuncia] 
getDenuncia conn = do
    query_ conn "SELECT * FROM denuncia LIMIT 1" :: IO [Denuncia] 

getNomeUsuarioId :: Connection -> Int64 -> IO String
getNomeUsuarioId conn userId = do
    [Only result] <- query conn "SELECT user_nome FROM usuario WHERE user_id = ?" (Only userId) :: IO [Only String]
    return result

getNomeJogoId :: Connection -> Int64 -> IO String
getNomeJogoId conn gameId = do
    [Only result] <- query conn "SELECT game_nome FROM jogo WHERE game_id = ?" (Only gameId) :: IO [Only String]
    return result

apagaDenuncia :: Connection -> Int64 -> IO()
apagaDenuncia conn denunciaId = do
    execute conn "DELETE FROM denuncia WHERE denuncia_id = ?" (Only denunciaId)
    return()

getGameId::Connection -> String -> IO Int64
getGameId conn nomeJogo = do 
    [Only result] <- query conn "SELECT game_id FROM jogo WHERE game_nome = ?" (Only nomeJogo) :: IO [Only Int64]
    return result