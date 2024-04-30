:- module(cliente, [menuCliente/1]).
:- use_module(util).
:- use_module("./LocalDB/ConnectionDB").
:- use_module("./LocalDB/DatabaseOperations").
:- use_module("Jogo").
:- use_module("Compra").
:- dynamic(current_user_id/1).
current_user_id(0).

/*
    | Seta o id do usuário como dinamico neste arquivo e chama a função princial do menu
    | do Usuário Padrão (Cliente)
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
    writeln("1 - Jogos Disponíveis"),
    writeln("2 - Mensagens"),
    writeln("3 - Meu Perfil"), % TODO
    writeln("4 - Sair"),
    writeln(""),
    writeln("================================================================================"),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, Opcao),
    limparTela,
    (
        Opcao = "1" -> jogosDisponiveis;
        Opcao = "2" -> mensagens;
        Opcao = "3" -> read_line_to_string(user_input, Undefine); %TODO
        Opcao = "4" -> limparTela;
        (
            printColorido("Opção inválida! Por favor, tente novamente.", red),
            exibeMenuCliente
        )
    ).

/* Exibe os filtros para a busca pelos jogos */
jogosDisponiveis :-
    writeln("================================================================================"),
    writeln("                                JOGOS DISPONÍVEIS                               "),
    writeln("================================================================================"),
    writeln("Filtrar por:"),
    writeln(""),
    writeln("1 - Nome"),
    writeln("2 - Gênero"),
    writeln("3 - Preço"),
    writeln("4 - Mais vendidos"),
    writeln("5 - Lançados Recentemente"),
    writeln("6 - Melhor Avaliados"),
    writeln("7 - Voltar"),
    writeln(""),
    writeln("================================================================================"),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, OpcaoJogosDisponiveis),
    opcaoJogosDisponiveis(OpcaoJogosDisponiveis).

/* Busca os jogos de acordo com os possíveis filtros listados anteriormente */
opcaoJogosDisponiveis("1") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Nome"]), writeln(""),
    get_connection(Connection),
    getJogosOrderByName(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
opcaoJogosDisponiveis("2") :-
    writeln("================================================================================"),
    write("Gêneros: "),
    get_connection(Connection),
    getTodosOsGeneros(Connection, Generos),
    print_generos(Generos),
    writeln("================================================================================"),
    writeln("Digite um gênero: "),
    read_line_to_string(user_input, Genero),
    limparTela,
    swritef(GeneroFiltrado, "Gênero (%w)", [Genero]),
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', [GeneroFiltrado]), writeln(""),
    getJogosByGender(Connection, Genero, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
opcaoJogosDisponiveis("3") :-
    limparTela,
    writeln("Opções: "),
    writeln(""),
    writeln("1 - Menor Preço"),
    writeln("2 - Maior Preço"),
    writeln("3 - Preço máximo"),
    writeln("4 - Preço mínimo"),
    writeln(""),
    printColorido("OBS: Para preços que possuem valores decimais separar a parte inteira da decimal com um ponto '.' (Ex: R$ 99.99)", yellow),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, FiltroPreco),
    filtroPreco(FiltroPreco), !.
opcaoJogosDisponiveis("4") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Mais vendidos"]), writeln(""),
    get_connection(Connection),
    getJogosMaisVendidos(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
opcaoJogosDisponiveis("5") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Lançados Recentemente"]), writeln(""),
    get_connection(Connection),
    getJogosOrderByDate(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
opcaoJogosDisponiveis("6") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Melhor Avaliados"]), writeln(""),
    get_connection(Connection),
    getJogosOrderByRating(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
opcaoJogosDisponiveis("7") :-
    limparTela,
    exibeMenuCliente, !.
/* Caso o filtro escolhido seja inválido, exibe a mensagem de erro e volta aos filtros */
opcaoJogosDisponiveis(_) :-
    limparTela,
    printColorido("Opção de filtro inválida! Por favor, tente novamente.", red),
    jogosDisponiveis.


/*
    | Filtros possíveis por preço caso a opção selecionada em jogos disponíveis seja a de número 3
    | 1 - Menor preço
    | 2 - Maior preço
    | 3 - Preço máximo
    | 4 - Preço mínimo
*/
filtroPreco("1") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Menor Preço"]), writeln(""),
    get_connection(Connection),
    getJogosOrderByPrice(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
filtroPreco("2") :-
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', ["Maior Preço"]), writeln(""),
    get_connection(Connection),
    getJogosOrderByBiggestPrice(Connection, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
filtroPreco("3") :-
    writeln("Digite um preço máximo: "),
    read_line_to_string(user_input, PrecoMaximo),
    swritef(PrecoMaximoFiltrado, "Preço máximo (R$ %w)", [PrecoMaximo]),
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', [PrecoMaximoFiltrado]), writeln(""),
    get_connection(Connection),
    getJogosUntilOnePrice(Connection, PrecoMaximo, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
filtroPreco("4") :-
    writeln("Digite um preço mínimo: "),
    read_line_to_string(user_input, PrecoMaximo),
    swritef(PrecoMaximoFiltrado, "Preço mínimo (R$ %w)", [PrecoMaximo]),
    limparTela,
    write("Jogos filtrados por: "), ansi_format([fg(blue)], '~w', [PrecoMaximoFiltrado]), writeln(""),
    get_connection(Connection),
    getJogosMinimumPrice(Connection, PrecoMaximo, Jogos),
    close_connection(Connection),
    exibeJogos(Jogos), !.
/* Caso o filtro por preço escolhido, seja inválido exibe a mensagem de erro e volta aos filtros */
filtroPreco(_) :-
    limparTela,
    printColorido("Opção de filtro por preço inválida! Por favor, tente novamente.", red),
    jogosDisponiveis, !.

/* Exibe todos os jogos de acordo com um filtro selecionado anteriormente */
exibeJogos([]) :-
    limparTela,
    printColorido("Nenhum jogo encontrado para esse filtro.", yellow),
    jogosDisponiveis, !.
exibeJogos(Jogos) :-
    print_jogos(Jogos),
    writeln("Digite um ID para ver os detalhes do jogo (ou tecle ENTER para sair):"),
    read_line_to_string(user_input, JogoID),
    limparTela,
    (
        % Volta à tela de filtros caso tecle ENTER
        JogoID = "" ->
            jogosDisponiveis
        ;
        (
            string_chars(JogoID, JogoIDArrayChars),
            validaJogoID(JogoIDArrayChars, Jogos, Result),
            % Se o JogoID é válido (Result = 1) então chama a etapa de listar todos os dados do jogo
            Result =:= 1 ->
            (
                get_connection(Connection),
                getJogosById(Connection, JogoID, [Jogo|_]),
                close_connection(Connection),
                exibeJogoCompleto(Jogo)
            )
            ;
            % Se o JogoID não é válido (Result = 0) então exibe a mensagem de erro e exibe a lista de mesmo filtro novamente
            (
                printColorido("ID de jogo inválido! Por favor, tente novamente utilizando outro ID.", red),
                exibeJogos(Jogos)
            )
        )
    ).

/*
    | Valida o ID de um jogo dentre um array de jogos que possuem em comum algum dos filtros anteriores.
    | A função verifica se existe algum jogo com o id passado como parâmetro na variável JogoIDArrayChars.
    | Se sim, Result será igual a 1, senão, 0.
*/
validaJogoID(_, [], 0) :- !.
validaJogoID(JogoIDArrayChars, [Jogo|OutrosJogos], Result) :-
    Jogo = row(GameIDString, _, _, _, _, _, _),
    string_chars(GameIDString, GameIDArrayChars),
    (
        JogoIDArrayChars == GameIDArrayChars ->
            Result = 1
        ;
            (
                validaJogoID(JogoIDArrayChars, OutrosJogos, NewResult),
                Result = NewResult
            )
    ).


/*
    | Exibe os dados detalhados de um jogo (ID, Nome, Gênero, Descricao, Data de lançamento(no formato: Ano, Mes, Dia), Avaliação, Preço).
    | Em seguida, pergunta ao usuário se deseja comprar o respectivo jogo. Se sim, a etapa de realizar a compra é chamada, senão, volta-se
    | aos filtros dos jogos disponíveis.
*/
exibeJogoCompleto(Jogo) :-
    print_jogo_detalhado_individual(Jogo),
    writeln('--------------------------------------------------------------------------------'),
    writeln("Deseja comprar esse jogo (s/n)?"),
    read_line_to_string(user_input, OpcaoCompra),
    string_lower(OpcaoCompra, OpcaoCompraLower),
    string_chars(OpcaoCompraLower, OpcaoCompraLowerArrayChar),
    limparTela,
    (
        OpcaoCompraLowerArrayChar == ['s'] ->
            get_connection(Connection),
            current_user_id(UserID),
            Jogo = row(JogoID, _, _, _, _, _, _),
            realizaCompra(Connection, JogoID, UserID),
            exibeMenuCliente
        ;
        OpcaoCompraLowerArrayChar == ['n'] ->
            jogosDisponiveis
        ;
        (
            printColorido("Opção inválida! Por favor, tente novamente.", red),
            exibeJogoCompleto(Jogo)
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
    | Checa se o nickname de um usuário que se deseja ver/enviar mensagens é válido (Result = 1) ou
    | não (Result = 0).
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
    