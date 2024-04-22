:- module(databaseOperations, [
    get_connection/1,
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
    db_query/3
]).
:- use_module(library(odbc)).

/* Busca a conexão com o banco de dados */
get_connection(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).


db_query(Connection, Query, Rows) :-
    findall(
        Result,
        odbc_query(Connection, Query, Result),
        Rows
    ).

db_query_no_return(Connection, Query) :-
    odbc_query(Connection, Query).

db_parameterized_query(Connection, Query, Parameters, Rows):-
    swritef(String, Query, Parameters),
    db_query(Connection, String, Rows).
    
db_parameterized_query_no_return(Connection, Query, Parameters):-
    swritef(String, Query, Parameters),
    db_query_no_return(Connection, String).

/* Consultas comumente utilizadas */

userAlreadyExistsById(Connection, Id) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [Id], [row(CountRow)]),
    (CountRow > 0).

userAlreadyExistsByNickname(Connection, Nickname) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_nickname = '%w'",
    db_parameterized_query(Connection, Q, [Nickname], [row(CountRow)]),
    (CountRow > 0).

userAlreadyExistsByEmail(Connection, Email) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_email = '%w'",
    db_parameterized_query(Connection, Q, [Email], [row(CountRow)]),
    (CountRow > 0).

checkExistsUserByEmailSenha(Connection, Email, Senha) :-
    Q = "SELECT COUNT(*) FROM usuario WHERE user_email = '%w' AND user_senha = '%w'",
    db_parameterized_query(Connection, Q, [Email, Senha], [row(CountRow)]),
    (CountRow > 0).


getUser(Connection, Email, Senha, User) :-
    Q = "SELECT * FROM usuario WHERE user_email = '%w' and user_senha = '%w'",
    db_parameterized_query(Connection, Q, [Email, Senha], User).

getUniqueDataRow([row(Data)], Data) :- !.

getUserNicknameById(Connection, UserID, Nickname) :-
    Q = "SELECT user_nickname FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], NicknameRow),
    getUniqueDataRow(NicknameRow, Nickname).

getUserNomeById(Connection, UserID, Nome) :-
    Q = "SELECT user_nome FROM usuario WHERE user_id = '%w'",
    db_parameterized_query(Connection, Q, [UserID], NomeRow),
    getUniqueDataRow(NomeRow, Nome).



getUserIdByNickname(Connection, UserNickname, UserID) :-
    Q = "SELECT user_id FROM usuario WHERE user_nickname = '%w'",
    db_parameterized_query(Connection, Q, [UserNickname], UserIDRow),
    getUniqueDataRow(UserIDRow, UserID).

getMensagensByUserIDFriendID(Connection, UserID, FriendID, Mensagens) :-
    Q = "SELECT id_remetente, message_texto FROM mensagem
         WHERE id_remetente IN (%w, %w) AND id_destinatario IN (%w,%w)
         ORDER BY message_date",
    db_parameterized_query(Connection, Q, [UserID, FriendID, UserID, FriendID], Mensagens).

enviarMensagem(Connection, UserID, FriendID, TextoNovaMensagem) :-
    Q = "INSERT INTO mensagem (id_remetente, id_destinatario, message_texto) values (%w, %w, '%w')",
    db_parameterized_query_no_return(Connection, Q, [UserID, FriendID, TextoNovaMensagem]).