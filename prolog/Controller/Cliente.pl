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
    writeln('1 - editar perfil'),
    writeln('2 - carteira'),

    read_line_to_string(user_input, Opcao),
    writeln(''),
    limparTela,

    (Opcao == "1" -> editarPerfil
    ;
     carteiraCliente).

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

    (
        Opcao == "1" -> selecionarQntSaldo;
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



formatarData(date(Ano, Mes, Dia), DataFormatada) :-
    format_time(string(DataFormatada), '%d/%m/%Y', date(Ano, Mes, Dia)).

editarPerfil :-
    get_connection(Connection),
    current_user_id(UserID),

    getUserNomeById(Connection, UserID, Nome),
    getUserNicknameById(Connection, UserID, Nick),
    getUserEmailById(Connection, UserID, Email),
    getUserTipoById(Connection, UserID, Tipo),
    getUserDataById(Connection, UserID, Date),

    formatarData(Date, DataFormatada),

    close_connection(Connection),

    write('================================================================================'), nl,
    write('                   Informações do seu Perfil:                   '), nl,
    write('================================================================================'), nl,
    write(''), nl,
    write('Nome: '), write(Nome), nl,
    write('Nickname: '), write(Nick), nl,
    write('Email: '), write(Email), nl,
    write('Senha: *******'), nl,
    write('Tipo de usuário: '), write(Tipo), write(' - Não alterável.'), nl,
    write('Data de criação: '), write(DataFormatada), write(' - Não alterável.'), nl,
    write(''), nl,
    write('================================================================================'), nl,
    write('1. Alterar Nome'), nl,
    write('2. Alterar Nickname'), nl,
    write('3. Alterar Email'), nl,
    write('4. Alterar Senha'), nl,
    write('5. Excluir conta'), nl,
    write('6. Voltar'), nl,
    write('================================================================================'), nl,
    write('Selecione uma opção > '),
    read_line_to_string(user_input, Opcao),
    writeln(''),
    limparTela,

    (
        Opcao == "1" -> alterarNome;
        Opcao == "2" -> alterarNick;
        Opcao == "3" -> alterarEmail;
        Opcao == "4" -> alterarSenha;
        Opcao == "5" -> excluirConta;
        Opcao == "6" -> perfilClienteTEMPORARIO;

        (
            printColorido("Opção inválida! Por favor, tente novamente.", red),
            editarPerfil
        )
    ).

alterarNome :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserNomeById(Connection, UserID, NomeString),

    writeln("================================================================================"),
    writeln("Insira o nome desejado: "),

    read_line_to_string(user_input, NomeDesejadoString),
    string_chars(NomeDesejadoString, NomeDesejado),
    string_chars(NomeString, NomeAtual),

    writeln(''),
    limparTela,

    (
        get_connection(Connection),
        not(verificaCamposNaoVazios([NomeDesejadoString])) ->
            printColorido("Nenhum campo pode estar vazio!", red),
            tentarNovamente("Deseja tentar novamente?", alterarNome)
        ;
        not(verificaCamposTamMaxStr50([NomeDesejadoString])) ->
            printColorido("Campos não podem ter mais que 50 caracteres!", red),
            tentarNovamente("Deseja tentar novamente?", alterarNome)
        ;
        NomeAtual == NomeDesejado ->
            tentarNovamente("OPS, o nome inserido é igual ao nome atual.", alterarNome)
        ;
        atualizaUserNome(Connection, UserID, NomeDesejadoString),
        writeln("================================================================================"),
        writeln("                         Nome atualizado com sucesso!                           "),
        writeln("================================================================================"),
        editarPerfil
    ),
        close_connection(Connection).

alterarNick :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserNicknameById(Connection, UserID, NickString),

    writeln("================================================================================"),
    writeln("Insira o nick desejado: "),

    read_line_to_string(user_input, NickDesejadoString),
    string_chars(NickDesejadoString, NickDesejado),
    string_chars(NickString, NickAtual),

    writeln(''),
    limparTela,

    (
        get_connection(Connection),
        not(verificaCamposNaoVazios([NickDesejadoString])) ->
            printColorido("Nenhum campo pode estar vazio!", red),
            tentarNovamente("Deseja tentar novamente?", alterarNick)
        ;
        not(verificaCamposTamMaxStr50([NickDesejadoString])) ->
            printColorido("Campos não podem ter mais que 50 caracteres!", red),
            tentarNovamente("Deseja tentar novamente?", alterarNick)
        ;
        userAlreadyExistsByNickname(Connection, NickDesejadoString) ->
            printColorido("Nickname já existe!", red),
            tentarNovamente("Deseja tentar novamente?", alterarNick)
        ;
        NickAtual == NickDesejado ->
            tentarNovamente("OPS, o nick inserido é igual ao nick atual.", alterarNick)
        ;
        atualizaUserNick(Connection, UserID, NickDesejadoString),
        writeln("================================================================================"),
        writeln("                         Nick atualizado com sucesso!                           "),
        writeln("================================================================================"),
        editarPerfil
    ),
        close_connection(Connection).

alterarEmail :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserEmailById(Connection, UserID, EmailString),

    writeln("================================================================================"),
    writeln("Insira o email desejado: "),

    read_line_to_string(user_input, EmailDesejadoString),
    string_chars(EmailDesejadoString, EmailDesejado),
    string_chars(EmailString, EmailAtual),

    writeln(''),
    limparTela,

    (
        get_connection(Connection),
        not(verificaCamposNaoVazios([EmailDesejadoString])) ->
            printColorido("Nenhum campo pode estar vazio!", red),
            tentarNovamente("Deseja tentar novamente?", alterarEmail)
        ;
        not(verificaCamposTamMaxStr50([EmailDesejadoString])) ->
            printColorido("Campos não podem ter mais que 50 caracteres!", red),
            tentarNovamente("Deseja tentar novamente?", alterarEmail)
        ;
        userAlreadyExistsByEmail(Connection, EmailDesejadoString) ->
            printColorido("Email já cadastrado!", red),
            tentarNovamente("Deseja tentar novamente?", alterarEmail)
        ;
        EmailAtual == EmailDesejado ->
            tentarNovamente("OPS, o email inserido é igual ao email atual.", alterarEmail)
        ;
        atualizaUserEmail(Connection, UserID, EmailDesejadoString),
        writeln("================================================================================"),
        writeln("                        Email atualizado com sucesso!                           "),
        writeln("================================================================================"),
        editarPerfil
    ),
        close_connection(Connection).


alterarSenha :-
    get_connection(Connection),
    current_user_id(UserID),
    getUserSenhaById(Connection, UserID, SenhaString),
    string_chars(SenhaString, SenhaAtual),

    solicitarSenha(SenhaAtual, ConfirmarSenha),

    (   ConfirmarSenha =:= 1 ->
            writeln("================================================================================"),
            writeln("Insira a senha desejada: "),

            read_line_to_string(user_input, SenhaDesejadaString),
            string_chars(SenhaDesejadaString, SenhaDesejada),
            

            writeln(''),
            limparTela,

            (
                get_connection(Connection),
                not(verificaCamposNaoVazios([SenhaDesejadaString])) ->
                    printColorido("Nenhum campo pode estar vazio!", red),
                    tentarNovamente("Deseja tentar novamente?", alterarSenha)
                ;
                not(verificaCamposTamMaxStr50([SenhaDesejadaString])) ->
                    printColorido("Campos não podem ter mais que 50 caracteres!", red),
                    tentarNovamente("Deseja tentar novamente?", alterarSenha)
                ;
                SenhaAtual == SenhaDesejada ->
                    tentarNovamente("OPS, a senha inserida é igual a senha atual.", alterarEmail)
                ;
                atualizaUserSenha(Connection, UserID, SenhaDesejadaString),
                writeln("================================================================================"),
                writeln("                      Senha atualizada com sucesso!                           "),
                writeln("================================================================================"),
                close_connection(Connection),
                editarPerfil
            )
        ;
        editarPerfil
    ).


excluirConta :-
    writeln("================================================================================"),
    printColorido("-----------------------------------Atenção--------------------------------------", red),
    printColorido("Excluir sua conta irá deletar todas suas mensagens, jogos, e qualquer outra", red),
    printColorido("informação ligada a sua conta.", red),
    writeln(""),
    printColorido("Esta ação NÃO PODE ser revertida!", red),
    writeln(""),
    writeln("Tem certeza que deseja continuar? (y/n) >"),

    read_line_to_string(user_input, Opcao),
    limparTela,
    string_lower(Opcao, Opcao_lower),

    (
        Opcao_lower == "y" ->
            get_connection(Connection),
            current_user_id(UserID),
            getUserSenhaById(Connection, UserID, SenhaString),
            string_chars(SenhaString, SenhaAtual),

            solicitarSenha(SenhaAtual, ConfirmarSenha),

            (   
                ConfirmarSenha =:= 1 ->
                    deletaUserConta(Connection, UserID),
                     writeln("================================================================================"),
                     writeln("                          Sua conta foi deletada!                               "),
                     writeln("================================================================================")
                    ;
                    editarPerfil
            )
        ;
        Opcao_lower == "n" ->
            editarPerfil
        ;
        (
            printColorido("Opção inválida!", red),
            editarPerfil
        )
    ).


tentarNovamente(Message, Acao1) :-
    writeln('================================================================================'),
    writeln(Message),
    writeln('================================================================================'),
    writeln('1. Tentar novamente'),
    writeln('2. Cancelar'),
    writeln('================================================================================'),
    writeln('Selecione uma opção > '),

    read_line_to_string(user_input, Opcao),
    limparTela,

    (
        Opcao == "1" -> call(Acao1)
        ;       
        Opcao == "2" -> editarPerfil;
        (
            printColorido("Opção inválida!", red),
            editarPerfil
        )
    ).

