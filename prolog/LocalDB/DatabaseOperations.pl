:- module(databaseOperations, [
    get_connection/1,
    close_connection/1,
    db_parameterized_query/4,
    db_parameterized_query_no_return/3,
    userAlreadyExistsById/2,
    userAlreadyExistsByNickname/2,
    userAlreadyExistsByEmail/2,
    checkExistsUserByEmailSenha/3,
    getUser/4,
    getUserNicknameById/3,
    getUserNomeById/3,
    getUserIdByNickname/3,
    getMensagensByUserIDFriendID/4,
    enviarMensagem/4,
    getUserSaldoById/3,
    getUserSenhaById/3,
    getInformacoesPerfil/3,
    getUserNomeById/3,
    getUserEmailById/3,
    getUserTipoById/3,
    getUserDataById/3,
    atualizaUserNome/3,
    atualizaUserNick/3,
    atualizaUserEmail/3,
    atualizaUserSenha/3,
    deletaUserConta/2
]).
:- use_module(library(odbc)).

/* Busca a conexão com o banco de dados */
get_connection(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).

/* Encerra a conexão com o banco de dados */
close_connection(Connection) :-
    odbc_disconnect(Connection).

/* Realiza uma consuta com o banco de dados e retorna um array com as instâncias obtidas na consulta */
db_query(Connection, Query, Rows) :-
    findall(
        Result,
        odbc_query(Connection, Query, Result),
        Rows
    ).

/* Realiza uma consuta sem retorno com o banco de dados */
db_query_no_return(Connection, Query) :-
    odbc_query(Connection, Query).

/*
    Refatora a consulta (Query) adicionando os valores dos parâmetros (Parameters) passados como argumento. Em seguida, chama a função que realiza a consulta e retorna as instâncias obtidas
    na consulta.
*/
db_parameterized_query(Connection, Query, Parameters, Rows):-
    swritef(String, Query, Parameters),
    db_query(Connection, String, Rows).
    
/*
    Refatora a consulta (Query) adicionando os valores dos parâmetros (Parameters) passados como argumento. Em seguida, chama a função que realiza a consulta sem retorno.
*/
db_parameterized_query_no_return(Connection, Query, Parameters):-
    swritef(String, Query, Parameters),
    db_query_no_return(Connection, String).

/* CONSULTAS COMUMENTE UTILIZADAS */

/* Verifica se já existe um usuário cadastrado com um determinado Id */
userAlreadyExistsById(Connection, Id) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [Id], [row(CountRow)]),
    (CountRow > 0).

/* Verifica se já existe um usuário cadastrado com um determinado Nickname */
userAlreadyExistsByNickname(Connection, Nickname) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_nickname = '%w'",
    db_parameterized_query(Connection, Q, [Nickname], [row(CountRow)]),
    (CountRow > 0).

/* Verifica se já existe um usuário cadastrado com um determinado Email */
userAlreadyExistsByEmail(Connection, Email) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_email = '%w'",
    db_parameterized_query(Connection, Q, [Email], [row(CountRow)]),
    (CountRow > 0).

/* Checa se existe um usuário cadastrado com algum determinado Email e Senha */
checkExistsUserByEmailSenha(Connection, Email, Senha) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_email = '%w' AND user_senha = '%w'",
    db_parameterized_query(Connection, Q, [Email, Senha], [row(CountRow)]),
    (CountRow > 0).

/* Busca todos os dados de um usuário de acordo com o seu Email e Senha */
getUser(Connection, Email, Senha, User) :-
    Q = "SELECT * FROM usuario WHERE user_email = '%w' and user_senha = '%w'",
    db_parameterized_query(Connection, Q, [Email, Senha], User).

/* Converte os retornos de consultas do tipo [row(UniqueData)] para o seu valor bruto (Data) */
getUniqueDataRow([row(Data)], Data) :- !.

/* Busca o nickname de um usuáiro de acordo com o seu Id */
getUserNicknameById(Connection, UserID, Nickname) :-
    Q = "SELECT user_nickname FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], NicknameRow),
    getUniqueDataRow(NicknameRow, Nickname).

/* Busca o saldo de um usuario de acordo com o seu Id */
getUserSaldoById(Connection, UserID, Saldo) :-
    Q = "SELECT user_saldo FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], SaldoRow),
    getUniqueDataRow(SaldoRow, Saldo).

/* Busca a senha de um usuario de acordo com o seu Id */
getUserSenhaById(Connection, UserID, Senha) :-
    Q = "SELECT user_senha FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], SenhaRow),
    getUniqueDataRow(SenhaRow, Senha).

/* Busca o nome de um usuáiro de acordo com o seu Id */
getUserNomeById(Connection, UserID, Nome) :-
    Q = "SELECT user_nome FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], NomeRow),
    getUniqueDataRow(NomeRow, Nome).

/* Busca o email do usuario de acordo com o seu Id */
getUserEmailById(Connection, UserID, Email) :-
    Q = "SELECT user_email FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], EmailRow),
    getUniqueDataRow(EmailRow, Email).

/* Busca o tipo do usuario de acordo com o seu Id */
getUserTipoById(Connection, UserID, Tipo) :-
    Q = "SELECT user_tipo FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], TipoRow),
    getUniqueDataRow(TipoRow, Tipo).

/* Busca a data de criação do usuario de acordo com o seu Id */
getUserDataById(Connection, UserID, Data) :-
    Q = "SELECT user_date FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], DateRow),
    getUniqueDataRow(DateRow, Data).

/* Busca o Id de um usuáiro de acordo com o seu Nickname */
getUserIdByNickname(Connection, UserNickname, UserID) :-
    Q = "SELECT user_id FROM usuario WHERE user_nickname = '%w'",
    db_parameterized_query(Connection, Q, [UserNickname], UserIDRow),
    getUniqueDataRow(UserIDRow, UserID).

atualizaUserNome(Connection, UserID, NomeDesejado) :-
    Q = "UPDATE usuario SET user_nome = '%w' WHERE user_id = '%w'",
    db_parameterized_query_no_return(Connection, Q, [NomeDesejado, UserID]).

atualizaUserNick(Connection, UserID, NickDesejado) :-
    Q = "UPDATE usuario SET user_nickname = '%w' WHERE user_id = '%w'",
    db_parameterized_query_no_return(Connection, Q, [NickDesejado, UserID]).

atualizaUserEmail(Connection, UserID, EmailDesejado) :-
    Q = "UPDATE usuario SET user_email = '%w' WHERE user_id = '%w'",
    db_parameterized_query_no_return(Connection, Q, [EmailDesejado, UserID]).

atualizaUserSenha(Connection, UserID, SenhaDesejada) :-
    Q = "UPDATE usuario SET user_senha = '%w' WHERE user_id = '%w'",
    db_parameterized_query_no_return(Connection, Q, [SenhaDesejada, UserID]).

deletaUserConta(Connection, UserID) :-
    Q1 = "DELETE FROM compra WHERE user_id = '%w';",
    Q2 = "DELETE FROM comentario WHERE id_usuario = '%w';",
    Q3 = "DELETE FROM denuncia WHERE id_usuario = '%w';",
    Q4 = "DELETE FROM mensagem WHERE id_remetente = '%w';",
    Q5 = "DELETE FROM usuario WHERE user_id = '%w';",
    format(atom(Query), '~w~w~w~w~w', [Q1, Q2, Q3, Q4, Q5]),
    db_parameterized_query_no_return(Connection, Query, [UserID, UserID, UserID, UserID, UserID]).

/* Busca todas as mensagens entre dois usuários de acordo com os seus IDs */
getMensagensByUserIDFriendID(Connection, UserID, FriendID, Mensagens) :-
    Q = "SELECT id_remetente, message_texto FROM mensagem
         WHERE id_remetente IN (%w, %w) AND id_destinatario IN (%w,%w)
         ORDER BY message_date",
    db_parameterized_query(Connection, Q, [UserID, FriendID, UserID, FriendID], Mensagens).

/* Cadastra uma nova mensagem entre dois usuários de acordo com seus IDs */
enviarMensagem(Connection, UserID, FriendID, TextoNovaMensagem) :-
    Q = "INSERT INTO mensagem (id_remetente, id_destinatario, message_texto) values (%w, %w, '%w')",
    db_parameterized_query_no_return(Connection, Q, [UserID, FriendID, TextoNovaMensagem]).