:- module(jogo, [
    getJogos/2,
    getJogosAdm/2,
    getJogosById/3,
    getJogosByIdAdm/3,
    getJogosByNome/3,
    getJogosUntilOnePrice/3,
    getJogosMinimumPrice/3,
    getJogosByGender/3,
    getJogosOrderByDate/2,
    getJogosOrderByDateAdm/2,
    getJogosOrderByName/2,
    getJogosOrderByNameAdm/2,
    getJogosOrderByPrice/2,
    getJogosOrderByPriceAdm/2,
    getJogosOrderByBiggestPrice/2,
    getJogosOrderByBiggestPriceAdm/2,
    getJogosOrderByRating/2,
    getJogosOrderByRatingAdm/2,
    getJogosMaisVendidos/2,
    print_jogos/1,
    print_jogo/1,
    print_jogo_detalhado/1,
    print_jogo_detalhado_individual/1,
    getPriceJogo/3,
    jogoExiste/2,
    jogoExisteAdm/2,
    getTodosOsGeneros/2,
    print_generos/1,
    getNomeAndIDJogosCliente/3,
    getAvaliacaoByGameIDUserId/4,
    registrarAvaliacao/4,
    checkJogoEstaFavoritado/4,
    favoritarJogo/3,
    desfavoritarJogo/3,
    registrarComentario/4,
    registrarDenuncia/5,
    printJogosIdNome/1,
    printJogoDetalhadoAdm/1,
    existeAlgumJogoVendido/1
]).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- use_module(library(date)).

/* Busca todos os jogos do sistema */
getJogos(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true",
    db_query(Connection, Q, Jogos).

getJogosAdm(Connection, Jogos) :-
    Q = "SELECT * FROM jogo",
    db_query(Connection, Q, Jogos).

/* Busca os jogos por um ID */
getJogosById(Connection, JogoId, Jogo):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true and game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], Jogo).

getJogosByIdAdm(Connection, JogoId, Jogo):-
    Q = "SELECT * FROM jogo WHERE game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], Jogo).

/* Busca os jogos por um nome */
getJogosByNome(Connection, Nome, Jogo):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true and LOWER(game_nome) = LOWER('%w')",
    db_parameterized_query(Connection, Q, [Nome], Jogo).

/* Busca todos os jogos até um determinado preço */
getJogosUntilOnePrice(Connection, Preco, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true and game_price <= %w",
    db_parameterized_query(Connection, Q, [Preco], Jogos).

/* Busca todos os jogos com um determinado preço mínimo */
getJogosMinimumPrice(Connection, Preco, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true and game_price >= %w",
    db_parameterized_query(Connection, Q, [Preco], Jogos).

/* Busca todos os jogos com algum gênero específico */
getJogosByGender(Connection, Gender, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true and LOWER(game_genero) = LOWER('%w')",
    db_parameterized_query(Connection, Q, [Gender], Jogos).

/* Busca todos os jogos ordenados pela data de lançamento (Do mais recente até o mais antigo) */
getJogosOrderByDate(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true ORDER BY game_data_lancamento DESC",
    db_query(Connection, Q, Jogos).

getJogosOrderByDateAdm(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_data_lancamento DESC",
    db_query(Connection, Q, Jogos).

/* Busca os jogos ordenados pelo nome em ordem lexicográfica */
getJogosOrderByName(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true ORDER BY game_nome",
    db_query(Connection, Q, Jogos).

getJogosOrderByNameAdm(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_nome",
    db_query(Connection, Q, Jogos).

/* Busca os jogos ordenados do menor preço até o maior preço */
getJogosOrderByPrice(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true ORDER BY game_price",
    db_query(Connection, Q, Jogos).

getJogosOrderByPriceAdm(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_price ASC",
    db_query(Connection, Q, Jogos).

/* Busca os jogos ordenados do maior preço até o menor preço */
getJogosOrderByBiggestPrice(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true ORDER BY game_price DESC",
    db_query(Connection, Q, Jogos).

getJogosOrderByBiggestPriceAdm(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_price DESC",
    db_query(Connection, Q, Jogos).

/* Busca os jogos ordenados de acordo com a quantidade de vendas (do mais vendido ao menos vendido) */
getJogosMaisVendidos(Connection, Jogos):-
    Q = "SELECT j.* FROM jogo j JOIN compra c ON j.game_id = c.game_id GROUP BY j.game_id ORDER BY COUNT(j.game_id) DESC",
    db_query(Connection, Q, Jogos).

existeAlgumJogoVendido(Connection):-
    Q = "SELECT COUNT(*) FROM jogo j JOIN compra c ON j.game_id = c.game_id GROUP BY j.game_id ORDER BY COUNT(j.game_id) DESC",
    db_query(Connection, Q, [row(CountRow)]),
    (CountRow > 0).

/* Busca os jogos ordenados de acordo com a avaliação (do mais avaliado ao menos avaliado) */
getJogosOrderByRating(Connection, Jogos):-
    Q = "SELECT * FROM jogo WHERE game_visibilidade = true ORDER BY game_avaliacao DESC",
    db_query(Connection, Q, Jogos).

getJogosOrderByRatingAdm(Connection, Jogos):-
    Q = "SELECT * FROM jogo ORDER BY game_avaliacao DESC",
    db_query(Connection, Q, Jogos).

/* Busca o preço de um jogo de acordo com o seu ID */
getPriceJogo(Connection, JogoId, Preco):-
    Q = "SELECT game_price FROM jogo WHERE game_visibilidade = true and game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], PrecoRow),
    getUniqueDataRow(PrecoRow, Preco).

/* Verifica se um jogo existe de acordo com o ID */
jogoExiste(Connection, JogoId):-
    Q = "SELECT COUNT(*) FROM jogo WHERE game_visibilidade = true and game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], [row(CountRow)]),
    (CountRow > 0).

jogoExisteAdm(Connection, JogoId):-
    Q = "SELECT COUNT(*) FROM jogo WHERE game_id = %w",
    db_parameterized_query(Connection, Q, [JogoId], [row(CountRow)]),
    (CountRow > 0).

/* Busca todos os gêneros de jogos cadastrados no sistema */
getTodosOsGeneros(Connection, Generos):-
    Q = "SELECT game_genero, COUNT(game_genero) FROM jogo GROUP BY game_genero",
    db_query(Connection, Q, Generos).

getNomeAndIDJogosCliente(Connection, UserID, Jogos) :-
    Q = "SELECT j.game_id, j.game_nome FROM jogo j INNER JOIN compra c ON j.game_id = c.game_id WHERE c.user_id = %w",
    db_parameterized_query(Connection, Q, [UserID], Jogos).

/* Exibe (em uma mesma linha separados por vígulas ',') os gêneros e a quantidade de jogos com esses gêneros passados como parâmetro */
print_generos([]) :- writeln("Nenhum gênero encontrado."), !.
print_generos([row(Genero, QuantidadeJogos)]) :-
    ansi_format([fg(blue)], "~w(~d).~n", [Genero, QuantidadeJogos]).
print_generos([row(Genero, QuantidadeJogos)|OutrosGeneros]) :-
    ansi_format([fg(blue)], "~w(~d), ", [Genero, QuantidadeJogos]),
    print_generos(OutrosGeneros).

/* Exibe a lista de jogos passados como parâmetro */
print_jogos([]) :-
    writeln('Nenhum jogo encontrado.').
print_jogos([Jogo|OutrosJogos]) :-
    writeln('================================================================================'),
    writeln('                                 LISTA DE JOGOS                                 '),
    writeln('================================================================================'),
    print_jogo(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    print_jogos_rest(OutrosJogos).
/* Exibe, um a um, os jogos da lista de jogos */
print_jogos_rest([]).
print_jogos_rest([Jogo|OutrosJogos]) :-
    print_jogo(Jogo),
    printColorido('Digite o ID do jogo para ver detalhes', green),
    writeln('--------------------------------------------------------------------------------'),
    print_jogos_rest(OutrosJogos).

/* Exibe o ID, nome, gênero e preço de um jogo passado como parâmetro */
print_jogo(row(ID, Nome, Genero, _, _, _, Preco, _)) :-
    format('ID: ~d~nNome: ~w~nGênero: ~w~nPreço: ~2f~n', [ID, Nome, Genero, Preco]).

/* Exibe todos os dados de um jogo além de perguntar em relação a compra do mesmo */
print_jogo_detalhado([]) :-
    writeln('Nenhum jogo encontrado.').
print_jogo_detalhado([Jogo|_]) :-
    print_jogo_detalhado_individual(Jogo),
    writeln('Comprar jogo? [s/n]'),
    writeln('--------------------------------------------------------------------------------').

/*
    |Exibe todos os dados de um único jogo passado como parâmetro (ID, Nome, Genero, Descricao,
    | Data de lançamento (no formatp: Ano, Mes, Dia), Avaliacao, Preco)
*/
print_jogo_detalhado_individual(row(ID, Nome, Genero, Descricao, date(Ano, Mes, Dia), Avaliacao, Preco, _)) :-
    writeln('================================================================================'),
    format('                              DETALHES DO JOGO [~d]                              ~n', [ID]),
    writeln('================================================================================'),
    format('ID: ~d~nNome: ~w~nGênero: ~w~nDescrição: ~w~nData de Lançamento: ~d/~d/~d~nAvaliação: ~1f~nPreço: ~2f~n', [ID, Nome, Genero, Descricao, Dia, Mes, Ano, Avaliacao, Preco]).

printJogosIdNome([Jogo|OutrosJogos]) :-
    writeln('================================================================================'),
    writeln('                                 LISTA DE JOGOS                                 '),
    writeln('================================================================================'),
    printJogoIdNome(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    printJogosIdNomeRest(OutrosJogos).

printJogoIdNome(row(ID, Nome, _, _, _, _, _, _)) :-
    format('ID: ~d~nNome: ~w', [ID, Nome]).

printJogosIdNomeRest([Jogo|OutrosJogos]):-
    printJogoIdNome(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    printJogosIdNomeRest(OutrosJogos).

printJogosIdNomeRest([]).

printJogoDetalhadoAdm(row(ID, Nome, Genero, Descricao, date(Ano, Mes, Dia), Avaliacao, Preco, Visibilidade)) :-
    converteVisibilidade(Visibilidade, VisibilidadeConvertida),
    writeln('================================================================================'),
    format('                              DETALHES DO JOGO [~d]                              ~n', [ID]),
    writeln('================================================================================'),
    format('ID: ~d~nNome: ~w~nGênero: ~w~nDescrição: ~w~nData de Lançamento: ~d/~d/~d~nAvaliação: ~1f~nPreço: ~2f~nVisibilidade: ~w~n', [ID, Nome, Genero, Descricao, Dia, Mes, Ano, Avaliacao, Preco, VisibilidadeConvertida]).

converteVisibilidade('1', "Visivel").
converteVisibilidade('0', "Oculto").

getAvaliacaoByGameIDUserId(Connection, JogoID, UserID, Nota) :-
    Q = "SELECT avaliacao_compra FROM compra WHERE game_id = %w and user_id = %w",
    db_parameterized_query(Connection, Q, [JogoID, UserID], Nota).

registrarAvaliacao(Connection, JogoID, UserID, Nota) :-
    Q = "UPDATE compra SET avaliacao_compra = %w WHERE game_id = %w and user_id = %w",
    db_parameterized_query_no_return(Connection, Q, [Nota, JogoID, UserID]).

checkJogoEstaFavoritado(Connection, JogoID, UserID, EstaFavoritado) :-
    Q = "SELECT favoritar_jogo FROM compra WHERE user_id = %w and game_id = %w",
    db_parameterized_query(Connection, Q, [UserID, JogoID], Row),
    getUniqueDataRow(Row, EstaFavoritado).

desfavoritarJogo(Connection, JogoID, UserID) :-
    Q = "UPDATE compra SET favoritar_jogo = false WHERE game_id = %w AND user_id = %w",
    db_parameterized_query_no_return(Connection, Q, [JogoID, UserID]).

favoritarJogo(Connection, JogoID, UserID) :-
    Q = "UPDATE compra SET favoritar_jogo = true WHERE game_id = %w AND user_id = %w",
    db_parameterized_query_no_return(Connection, Q, [JogoID, UserID]).

registrarComentario(Connection, UserID, JogoID, Comentario) :-
    /* Transformando a data atual para o formato YYYY-MM-DD */
    get_time(TStamp),
    format_time(string(Txt),'%FT%T%z',TStamp),
    split_string(Txt, "T", "", DataSplitada),
    nth0(0, DataSplitada, DataFormatada),
    Q = "INSERT INTO comentario(id_usuario, id_jogo, comentario_texto, comentario_date) VALUES ('%w', '%w', '%w', '%w')",
    db_parameterized_query_no_return(Connection, Q, [UserID, JogoID, Comentario, DataFormatada]).

registrarDenuncia(Connection, UserID, JogoID, Motivo, Descricao) :-
    /* Transformando a data atual para o formato YYYY-MM-DD */
    get_time(TStamp),
    format_time(string(Txt),'%FT%T%z',TStamp),
    split_string(Txt, "T", "", DataSplitada),
    nth0(0, DataSplitada, DataFormatada),
    Q = "INSERT INTO denuncia(id_usuario, id_jogo, denuncia_motivo, denuncia_descricao, denuncia_data) VALUES (%w, %w, %w, %w, %w)",
    db_parameterized_query_no_return(Connection, Q, [UserID, JogoID, Motivo, Descricao, DataFormatada]).
