:- module(cliente, [menuCliente/1]).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- dynamic(current_user_id/1).
current_user_id(0).

/*
    Seta o id do usuário como dinamico neste arquivo e chama a função princial do menu
    do Usuário Padrão (Cliente)
*/
menuCliente(UserID) :-
    asserta(current_user_id(UserID)), % Guarda o ID do usuario atual
    exibeMenuCliente.

/* Exibe o Menu principal do Usuário Padrão */
exibeMenuCliente :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserNomeById(Connection, UserID, Nome),
    close_connection(Connection),
    writeln("================================================================================"),
    writeln("                                      HOME                                      "),
    writeln("================================================================================"),
    write("Seja bem vindo(a) "), ansi_format([fg(blue)], '~w', [Nome]), writeln("!"),
    writeln("O que deseja fazer?"),
    writeln(""),
    writeln("1 - Jogos Disponíveis"), % TODO
    writeln("2 - Mensagens"),
    writeln("3 - Meu Perfil"), % TODO
    writeln("4 - Sair"),
    writeln(""),
    writeln("================================================================================"),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, Opcao),
    escolherOpcao(Opcao).
/* Caso a opção do menu do cliente seja '2', limpa a tela e vai para a tela de mensagens */
escolherOpcao("2") :-
    limparTela,
    mensagens.
/* Caso a opção do menu do cliente seja '4', limpa a tela e e volta ao menu principal do sistema */
escolherOpcao("4") :-
    limparTela.
/* Caso a opção do menu inicial seja inválida exibe uma mensagem de erro e retorna ao menu do cliente */
escolherOpcao(_) :-
    limparTela,
    printColorido("Opção inválida! Por favor, tente novamente.", red),
    exibeMenuCliente.

/* Receber um nickname de usuário para exibir as mensagens */
mensagens :-
    writeln("================================================================================"),
    writeln("Digite o nickname do usuário para quem deseja enviar a mensagem:"),
    printColorido("(Obs: para sair basta teclar ENTER sem digitar nada)", yellow),
    read_line_to_string(user_input, FriendNickname),
    limparTela,
    current_user_id(UserID),
    (
        FriendNickname == "" ->
            exibeMenuCliente
        ;
            validaNicknameFriendMensagem(UserID, FriendNickname, Result),
            (
                Result =:= 1 ->
                    abrirChat(FriendNickname)
                ;
                    mensagens
            )
    ).

/* 
    Checa se o nickname de um usuário que se deseja ver/enviar mensagens é válido (Result = 1) ou
    não (Result = 0).
*/
validaNicknameFriendMensagem(UserID, FriendNicknameString, Result) :-
    get_connection(Connection),
    getUserNicknameById(Connection, UserID, UserNicknameString),
    string_chars(UserNicknameString, UserNickname),
    string_chars(FriendNicknameString, FriendNickname),
    (
        FriendNickname == UserNickname ->
            printColorido("O nickname digitado não pode ser seu próprio nickname!", red),
            printColorido("Tente novamente utilizando outro nickname.", red),
            Result = 0
        ;
        not(userAlreadyExistsByNickname(Connection, FriendNicknameString)) ->
            printColorido("Não existe usuário cadastrado com esse nickname!", red),
            printColorido("Tente novamente utilizando outro nickname.", red),
            Result = 0
        ;
            Result = 1
    ),
    close_connection(Connection).

/* Abre o chat de conversas entre usuários */
abrirChat(FriendNickname) :-
    %limparTela,
    writeln("================================================================================"),
    writeln("                                     CHAT                                       "),
    writeln("================================================================================"),
    get_connection(Connection),
    current_user_id(UserID),
    getUserIdByNickname(Connection, FriendNickname, FriendID),
    getMensagensByUserIDFriendID(Connection, UserID, FriendID, Mensagens),
    length(Mensagens, LengthMensagens),
    (
        LengthMensagens =:= 0 ->
            writeln("Sem mensagens entre esse usuário")
        ;
            writeln(""),
            exibeMensagens(UserID, FriendNickname, Mensagens)
    ),
    writeln("================================================================================"),
    writeln("Escreva uma mensagem (ou tecle ENTER para sair):"),
    read_line_to_string(user_input, NovaMensagem),
    (
        NovaMensagem == "" ->
            limparTela,
            exibeMenuCliente
        ;
            enviarMensagem(Connection, UserID, FriendID, NovaMensagem),
            limparTela,
            abrirChat(FriendNickname)
    ).

/* Exibe as mensagens com o usuário de acordo com o nickname amigo */
exibeMensagens(_, _, []).
exibeMensagens(UserID, FriendNickname, [row(IdRemetente, MensagemTexto) | Outras]) :-
    (
        UserID =:= IdRemetente ->
            ansi_format([fg(green)], '~w ', ["[Você]:"]),
            writeln(MensagemTexto),
            writeln("")
        ;
            ansi_format([fg(230, 0, 0)], '[~w]: ', [FriendNickname]),
            writeln(MensagemTexto),
            writeln("")
    ),
    exibeMensagens(UserID, FriendNickname, Outras).
    