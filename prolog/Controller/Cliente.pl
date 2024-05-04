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
    writeln("3 - Meu Perfil"),
    writeln("4 - Sair"),
    writeln(""),
    writeln("================================================================================"),
    writeln("Selecione uma opção: "),
    read_line_to_string(user_input, Opcao),
    limparTela,
    (
        Opcao = "1" -> jogosDisponiveis;
        Opcao = "2" -> mensagens;
        Opcao = "3" -> perfilCliente;
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
    Jogo = row(GameIDString, _, _, _, _, _, _, _),
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
            Jogo = row(JogoID, _, _, _, _, _, _, _),
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








perfilCliente :-
    current_user_id(UserID),
    get_connection(Connection),
    getUserNicknameById(Connection, UserID, Nickname),
    close_connection(Connection),
    (   Nickname \= 'invalid_user_id' ->
            writeln('================================================================================'),
            write(Nickname), writeln('\'s Perfil'),
            writeln('================================================================================'),
            writeln(''),
            writeln('1 - Meus jogos'),
            writeln('2 - Minha carteira'),
            writeln('3 - Editar perfil'),
            writeln('4 - Voltar'),
            writeln(''),
            writeln('================================================================================'),
            writeln('Selecione uma opção > '),

            read_line_to_string(user_input, Opcao),
            limparTela, 
            
            (   Opcao = "1" -> jogosCliente(UserID);
                Opcao = "2" -> carteiraCliente;
                Opcao = "3" -> editarPerfil;
                Opcao = "4" -> exibeMenuCliente;
                writeln('Opção inválida! Por favor, tente novamente.'),
                perfilCliente
            )
    ;   writeln('ID Usuário Inválido')
    ).

jogosCliente(UserID) :-
    % É preciso implementar o predicado que recupera os jogos do usuário a partir do BD. Jogos = [123, 245],
    get_connection(Connection),
    getNomeAndIDJogosCliente(Connection, UserID, Jogos),
    close_connection(Connection),
    writeln("================================================================================"),
    writeln("                                Meus Jogos                                      "),
    writeln("================================================================================"),
    exibirJogosCliente(Jogos),
    nl,
    
    writeln('Digite "sair" para voltar'),
    writeln('Insira o ID do jogo que deseja acessar:'),
    read_line_to_string(user_input, Input),
    limparTela,
    
    (   Input = "sair" -> perfilCliente;
        checkComprouJogo(UserID, Input, Result),
        (
            Result =:= 1 ->
                acessarJogo(UserID, Input)
            ;
            writeln('ID do jogo inválido.'),
            jogosCliente(UserID)
        )
    ).

checkComprouJogo(UserID, JogoID, Result) :-
    get_connection(Connection),
    (
        naoComprouJogo(Connection, JogoID, UserID) -> Result = 0; Result = 1
    ),
    close_connection(Connection).



acessarJogo(UserID, JogoID) :-
    writeln('Opções disponíveis para o jogo:'),
    writeln('1. Avaliar o jogo'),
    writeln('2. Favoritar o jogo'),
    writeln('3. Deixar um comentário'),
    writeln('4. Denunciar o jogo'),
    writeln('5. Voltar'),
    writeln('Digite o número da opção desejada:'),

    read_line_to_string(user_input, Opcao),
    limparTela,
    
    (   Opcao = "1" -> avaliarJogo(UserID, JogoID), acessarJogo(UserID, JogoID);
        Opcao = "2" -> favoritarJogo(UserID, JogoID), acessarJogo(UserID, JogoID);
        Opcao = "3" -> comentarJogo(Connection, UserID, JogoID), acessarJogo(UserID, JogoID);
        Opcao = "4" -> denunciarJogo(Connection, JogoID, UserID), acessarJogo(UserID, JogoID);
        Opcao = "5" -> jogosCliente(UserID);
        writeln('Opção inválida.')
    ).

avaliarJogo(UserID, JogoID) :-
   
    writeln('================================================================================'),
    writeln('Insira uma nota de 0 a 10 > '),

    read(Nota),
    (   number(Nota),
        0 =< Nota,
        Nota =< 10 
        
        -> (    
                get_connection(Connection),
                registrarAvaliacao(Connection, JogoID, UserID, Nota),
                close_connection(Connection),
                limparTela,
                writeln('================================================================================'),
                writeln('                         Jogo avaliado com sucesso                              '),
                writeln('================================================================================')
            )
        
        ;  (
                limparTela,
                writeln('Entrada inválida. Por favor, insira um valor dentro da faixa'),
                avaliarJogo(UserID, JogoID)
            )
    ).

favoritarJogo(UserID, JogoID) :-
    % Supus que exista fora de cliente.pl. Atentar também para o predicado 'execute' que é chamado dentro da função.
    get_connection(Connection),
    checkJogoEstaFavoritado(Connection, JogoID, UserID, EstaFavoritado),
    (   EstaFavoritado == '1' ->
        writeln('================================================================================'),
        writeln('O jogo já está em seus favoritos'),
        writeln('Deseja removê-lo dos favoritos? (s/n) >'),

        read_line_to_string(user_input, Opcao),
        limparTela,

        (   string_lower(Opcao, 's') ->
            
            desfavoritarJogo(Connection, JogoID, UserID),
            writeln('================================================================================'),
            writeln('Jogo removido dos favoritos!')
        ;   string_lower(Opcao, 'n') ->
            true
        ;   writeln('Opção inválida!')
        )
    ;   favoritarJogo(Connection, JogoID, UserID),
        writeln('================================================================================'),
        writeln('Jogo favoritado com sucesso')
    ),
    close_connection(Connection).

comentarJogo(Connection, UserID, JogoID) :-
    writeln('================================================================================'),
    writeln('Insira seu comentário ou digite "sair" para voltar >'),

    read_line_to_string(user_input, Comentario),
    limparTela,

    (   Comentario = sair -> acessarJogo(Connection, UserID, JogoID);
        %   aqui eu supus que vai haver algum predicado registrarComentario/4 que faça a inserção do comentário no BD.
        registrarComentario(Connection, UserID, JogoID, Comentario)
    ).

denunciarJogo(Connection, JogoID, UserID) :-
    writeln('================================================================================'),
    writeln('                                  Denúncia                                      '),
    writeln('================================================================================'),
    writeln(''),
    writeln('1. Não funciona/possui algum problema crítico'),
    writeln('2. Contém vírus/malwares'),
    writeln('3. Viola leis judiciais'),
    writeln('4. Conteúdo adulto não classificado'),
    writeln('5. Fraude (tenta obter dados de forma fraudulenta)'),
    writeln('6. Outros'),
    writeln('7. Voltar'),
    writeln(''),
    writeln('================================================================================'),
    writeln('Selecione o motivo da denúncia > '),

    read(Opcao),
    limparTela,

    escolherDenuncia(Opcao, Connection, JogoID, UserID).

escolherDenuncia("1", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Não funciona/possui algum problema crítico"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("2", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Contém vírus/malwares"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("3", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Viola leis judiciais"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("4", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Conteúdo adulto não classificado"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("5", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Fraude"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("6", Connection, JogoID, UserID) :-
    registrarDenuncia(Connection, JogoID, UserID, "Outros"),
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia("7", Connection, JogoID, UserID) :-
    acessarJogo(Connection, UserID, JogoID).

escolherDenuncia(_, Connection, JogoID, UserID) :-
    writeln('Opção inválida! Por favor, tente novamente.'),
    acessarJogo(Connection, UserID, JogoID).

% Não entendi bem como deve ser a exibição em exibirJogosCliente. Atualmente imprimimos os id: reformular post.
exibirJogosCliente([]).
exibirJogosCliente([Jogo|Resto]) :-
    Jogo = row(GameID, GameNome),
    format("~w | ~w ~n", [GameID, GameNome]),
    exibirJogosCliente(Resto).










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
        Opcao == "2" -> perfilCliente;
     
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
        Opcao == "6" -> perfilCliente;

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


