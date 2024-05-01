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
     limparTela,
    (
        Opcao == "1" -> jogosDisponiveis;
        Opcao == "2" -> mensagens;
        Opcao == "3" -> perfilClienteTEMPORARIO;
        Opcao == "4" -> limparTela;
        (
            printColorido("Opção inválida! Por favor, tente novamente.", red),
            exibeMenuCliente
        )
    ).

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


perfilClienteTEMPORARIO :-
    writeln('Perfil TEMPORARIO'),
    writeln('1 - carteira'),
    writeln('2 - editar perfil'),

    read_line_to_string(user_input, Opcao),
    writeln(''),
    limparTela,

    (Opcao == "1" -> carteiraCliente;
     Opcao == "2" -> editarPerfil).

carteiraCliente :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserSaldoById(Connection, UserID, Saldo),
    close_connection(Connection),

    format('================================================================================~n'),
    format('                              Sua Carteira                                      ~n'),
    format('================================================================================~n'),
    format('~n'),
    format('             ╔══════════════════════════════════════════════╗~n'),
    format('             ║                                              ║~n'),
    format('               Saldo Disponível: R$~2f~n', [Saldo]),
    format('             ║                                              ║~n'),
    format('               1. Adicionar Saldo                           ~n'),
    format('             ║ 2. Voltar                                    ║~n'),
    format('             ║                                              ║~n'),
    format('             ╚══════════════════════════════════════════════╝~n'),
    format('~n'),
    format('================================================================================~n'),
    format('Selecione uma opção > '),

    read_line_to_string(user_input, Opcao),
    writeln(''),
    limparTela,

    (Opcao == "1" -> selecionarQntSaldo;
     Opcao == "2" -> perfilClienteTEMPORARIO;
     
        (
        printColorido("Opção inválida! Por favor, tente novamente.", red),
        carteiraCliente
        )
     ).
    

selecionarQntSaldo :-
    writeln('================================================================================'),
    writeln(' Quanto deseja adicionar?                                   '),
    writeln(''),
    writeln('1 - R$ 5,00'),
    writeln('2 - R$ 10,00'),
    writeln('3 - R$ 20,00'),
    writeln('4 - R$ 50,00'),
    writeln('5 - R$ 100,00'),
    writeln('6 - Voltar'),
    writeln(''),
    writeln('================================================================================'),
    writeln('Selecione uma opção > '),

    read_line_to_string(user_input, Opcao),
    limparTela,

    (
        Opcao == "1" -> confirmacaoAddSaldo(5.00);
        Opcao == "2" -> confirmacaoAddSaldo(10.00);
        
        Opcao == "3" -> confirmacaoAddSaldo(20.00);
        Opcao == "4" -> confirmacaoAddSaldo(50.00);
        
        Opcao == "5" -> confirmacaoAddSaldo(100.00);
        Opcao == "6" -> carteiraCliente;

        (
            printColorido("Opção inválida! Por favor, tente novamente.", red),
            selecionarQntSaldo
        )
    ).

confirmacaoAddSaldo(Saldo) :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserSenhaById(Connection, UserID, SenhaAtualString),
    close_connection(Connection),

    string_chars(SenhaAtualString, SenhaAtual),

    solicitarSenha(SenhaAtual, ConfirmarSenha),

    (   
        ConfirmarSenha =:= 1 ->
            addSaldoByID(Saldo),
            carteiraCliente
        ;
            selecionarQntSaldo
    ).

solicitarSenha(SenhaAtual, ConfirmarSenha) :-

    writeln('================================================================================'),
    writeln('Confirme sua senha atual:'),
    read_line_to_string(user_input, SenhaDigitadaString),
    string_chars(SenhaDigitadaString, SenhaDigitada),

    (
        SenhaAtual == SenhaDigitada -> 
            ConfirmarSenha is 1
        ;
        writeln('================================================================================'),
        printColorido("SENHA INCORRETA", red),
        writeln('================================================================================'),
        writeln('1. Tentar novamente'),
        writeln('2. Cancelar'),
        writeln('================================================================================'),
        writeln('Selecione uma opção > '),

        read_line_to_string(user_input, Opcao),
        limparTela,

        (
            Opcao == "1" -> solicitarSenha(SenhaAtual, ConfirmarSenha);
            Opcao == "2" -> ConfirmarSenha is 0;
            (
                printColorido("Opção inválida! Por favor, tente novamente.", red),
                ConfirmarSenha is 0
            )
        )
    ).

addSaldoByID(SaldoAdicionar) :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserSaldoById(Connection, UserID, SaldoAtual),
    
    SaldoAtualizado is SaldoAtual + SaldoAdicionar, 
    Q = 'UPDATE usuario SET user_saldo = %w WHERE user_id = %w',
    db_parameterized_query_no_return(Connection, Q, [SaldoAtualizado, UserID]),
    close_connection(Connection),

    limparTela,
    writeln('================================================================================'),
    writeln('                        Saldo atualizado com sucesso!                           '),
    writeln('================================================================================').
    

