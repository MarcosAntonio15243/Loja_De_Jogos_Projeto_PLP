:- module(menuinicial, [
    menuInicial/0
]).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations", [
    get_connection/1,
    userAlreadyExistsByNickname/2,
    userAlreadyExistsByEmail/2,
    db_parameterized_query_no_return/3
]).
:- use_module(library(date)).

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

/*
escolherOpcao("1") :-
    login.
*/

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
    writeln('Opção inválida. Por favor, escolha novamente.'),
    menuInicial.

/*
login :- 
    limparTela
    writeln("================================================================================"),
    writeln("                                      LOGIN                                     "),
    writeln("================================================================================"),
    writeln("Preencha seus dados abaixo:"),
    writeln(""),
    writeln("Digite o e-mail:"),
    read_line_to_string(user_input, Email),
    writeln("Digite a senha:"),
    read_line_to_string(user_input, Senha),
    writeln("================================================================================").
*/

criarConta :-
    get_connection(Connection),
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
    read_line_to_string(user_input, Senha),
    writeln("Confirmar Senha (máximo de 50 caracteres):"),
    read_line_to_string(user_input, ConfirmarSenha),
    writeln("================================================================================"),
    limparTela,
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
        userAlreadyExistsByNickname(Connection, Nickname) ->
            printColorido("Nickname já existe!", red),
            desejaContinuarCriarConta
        ;
        userAlreadyExistsByEmail(Connection, Email) ->
            printColorido("Email já cadastrado!", red),
            desejaContinuarCriarConta
        ;
            cadastrarConta(Connection, Nickname, Nome, Email, Senha, Result),
            limparTela,
            (Result =:= 1) ->
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


cadastrarConta(Connection, Nickname, Nome, Email, Senha, Result) :-
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

        Result is 1.