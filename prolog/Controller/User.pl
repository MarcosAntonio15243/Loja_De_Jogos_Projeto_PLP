:- module(menuinicial, [
    menuInicial/0
]).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- use_module(library(date)).
:- use_module("./Admin", [menuAdmin/0]).
:- use_module("./Cliente", [menuCliente/1]).

menuInicial :-
    writeln("================================================================================"),
    writeln("                          BEM VINDO(A) À LOJA DE JOGOS                          "),
    writeln("================================================================================"),
    writeln("MENU:"),
    writeln(""),
    writeln(" 1 - Login"),
    writeln(" 2 - Criar uma conta"),
    writeln(" 3 - Sair"),
    writeln(""),
    writeln("================================================================================"),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, Opcao),
    escolherOpcao(Opcao).


escolherOpcao("1") :-
    limparTela,
    login.

escolherOpcao("2") :-
    limparTela,
    criarConta.

escolherOpcao("3") :-
    limparTela,
    writeln("╔══════════════════════════════════════════════════════════════════════════════╗"),
    writeln("║                                                                              ║"),
    writeln("║                    OBRIGADO POR UTILIZAR O NOSSO SISTEMA!                    ║"),
    writeln("║                                                                              ║"),
    writeln("║══════════════════════════════════════════════════════════════════════════════║"),
    writeln("║                                                                              ║"),
    writeln("║                                NOSSA EQUIPE:                                 ║"),
    writeln("║                                                                              ║"),
    writeln("║                                HILDON REGIS                                  ║"),
    writeln("║                                LEILA FARIAS                                  ║"),
    writeln("║                               MARCOS ANTONIO                                 ║"),
    writeln("║                               MARCOS VINÍCIUS                                ║"),
    writeln("║                                 JOÃO VICTOR                                  ║"),
    writeln("║                                                                              ║"),
    writeln("╚══════════════════════════════════════════════════════════════════════════════╝"),
    halt.

escolherOpcao(_) :-
    limparTela,
    printColorido("Opção inválida! Por favor, tente novamente.", red),
    menuInicial.


login :- 
    writeln("================================================================================"),
    writeln("                                      LOGIN                                     "),
    writeln("================================================================================"),
    writeln("Preencha seus dados abaixo:"),
    writeln(""),
    writeln("Digite o e-mail:"),
    read_line_to_string(user_input, Email),
    writeln("Digite a senha:"),
    read_line_to_string(user_input, Senha),
    writeln("================================================================================"),
    limparTela,
    (
        not(verificaCamposNaoVazios([Email, Senha])) ->
            printColorido("Nenhum campo pode estar vazio!", red),
            desejaContinuarLogin
        ;
        autenticaUser(Email, Senha, UserID, UserTipo, Autenticado),
        ( Autenticado =:= 1 ->
            (UserTipo = 'Padrão' ->
                menuCliente(UserID) % Transição para as telas de Usuário Padrão
            ;
                menuAdmin
            ),
            menuInicial
        ;
            printColorido("Email ou senha incorretos!", red),
            desejaContinuarLogin
        )
    ).

autenticaUser(Email, Senha, UserID, UserTipo, Autenticado) :-
    get_connection(Connection),
    getUser(Connection, Email, Senha, User),
    (User = [Row|_],
     Row = row(UserID, _, _, _, _, UserTipo, _, _) ->
        Autenticado = 1
    ;
        Autenticado = 0
    ),
    encerrandoDatabase(Connection).


desejaContinuarLogin :-
    writeln("Deseja continuar? (s/n)"),
    read_line_to_string(user_input, Opcao),
    limparTela,
    desejaContinuarLoginOpcao(Opcao).
desejaContinuarLoginOpcao(Opcao) :- (Opcao == "s"; Opcao == "S"), login.
desejaContinuarLoginOpcao(Opcao) :- (Opcao == "n"; Opcao == "N"), menuInicial.
desejaContinuarLoginOpcao(_) :- 
    limparTela,
    printColorido("Opção inválida!", red),
    desejaContinuarLogin.

criarConta :-
    writeln("================================================================================"),
    writeln("                                 CRIAR CONTA                                    "),
    writeln("================================================================================"),
    writeln("Preencha seus dados abaixo:\n"),
    writeln("Nickname (máximo de 50 caracteres):"),
    read_line_to_string(user_input, Nickname),
    writeln("Nome (máximo de 50 caracteres):"),
    read_line_to_string(user_input, Nome),
    writeln("E-mail (máximo de 50 caracteres):"),
    read_line_to_string(user_input, Email),
    writeln("Senha (máximo de 50 caracteres):"),
    read_line_to_string(user_input, SenhaString),
    writeln("Confirmar Senha (máximo de 50 caracteres):"),
    read_line_to_string(user_input, ConfirmarSenhaString),
    writeln("================================================================================"),
    limparTela,
    string_chars(SenhaString, Senha),
    string_chars(ConfirmarSenhaString, ConfirmarSenha),
    (
        not(verificaCamposNaoVazios([Nickname, Nome, Email, Senha, ConfirmarSenha])) ->
            printColorido("Nenhum campo pode estar vazio!", red),
            desejaContinuarCriarConta
        ;
        not(verificaCamposTamMaxStr50([Nickname, Nome, Email, Senha, ConfirmarSenha])) ->
            printColorido("Campos não podem ter mais que 50 caracteres!", red),
            desejaContinuarCriarConta
        ;
        Senha \== ConfirmarSenha ->
            printColorido("Senhas digitadas não são iguais!", red),
            desejaContinuarCriarConta
        ;
            cadastrarConta(Nickname, Nome, Email, Senha, Result),
            (Result =:= 1) ->
                limparTela,
                printColorido("Cadastro realizado com sucesso!", green),
                menuInicial
            ;
                desejaContinuarCriarConta
    ).
    
desejaContinuarCriarConta :-
    writeln("Deseja continuar? (s/n)"),
    read_line_to_string(user_input, Opcao),
    limparTela,
    desejaContinuarCriarContaOpcao(Opcao).
desejaContinuarCriarContaOpcao(Opcao) :- (Opcao == "s"; Opcao == "S"), criarConta.
desejaContinuarCriarContaOpcao(Opcao) :- (Opcao == "n"; Opcao == "N"), menuInicial.
desejaContinuarCriarContaOpcao(_) :- 
    limparTela,
    printColorido("Opção inválida!", red),
    desejaContinuarCriarConta.


cadastrarConta(Nickname, Nome, Email, Senha, Result) :-
    get_connection(Connection),
    (
        userAlreadyExistsByNickname(Connection, Nickname) ->
            printColorido("Nickname já existe!", red),
            Result is 0
        ;
        userAlreadyExistsByEmail(Connection, Email) ->
            printColorido("Email já cadastrado!", red),
            Result is 0
        ;
        /* Transformando a data atual para o formato YYYY-MM-DD */
        get_time(TStamp),
        format_time(string(Txt),'%FT%T%z',TStamp),
        split_string(Txt, "T", "", DataSplitada),
        nth0(0, DataSplitada, DataFormatada),
        
        Q = "INSERT INTO usuario (user_nickname, user_nome, user_email, user_senha, user_tipo, user_date, user_saldo) values (%w, %w, %w, %w, %w, %w, %w)",
        db_parameterized_query_no_return(Connection, Q, [Nickname, Nome, Email, Senha, "Padrão", DataFormatada, 0]),
        Result is 1
    ),
    encerrandoDatabase(Connection).