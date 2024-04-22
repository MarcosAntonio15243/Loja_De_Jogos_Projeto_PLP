:- module(telaAdmin, [menuAdmin/0]).
:- use_module("../LocalDB/ConnectionDB").
:- use_module("../LocalDB/DatabaseOperations").
:- use_module(library(date)).
:- use_module("../util").

menuAdmin :-
    writeln(''),
    writeln("================================================================================"),
    writeln("                             HOME - ADMINISTRADOR                               "),
    writeln("================================================================================"),
    writeln('|1. Adicionar Jogo                  |'),
    writeln('|2. Exibir Jogo                     |'),
    writeln('|3. Atualizar Jogo                  |'),
    writeln('|4. Remover Jogo                    |'),
    writeln('|5. Exibir Denuncia                 |'),
    writeln('|6. DashBoard                       |'),
    writeln('|7. Sair                            |'),
    writeln(''),
    writeln('Escolha uma opção: '),
    read_line_to_string(user_input, Opcao),
    writeln(''),
    (Opcao == "1" -> cadastraJogo;
     Opcao == "0" -> writeln('Saindo...'), writeln(''), halt).

cadastraJogo :-
    get_connection(Connection),
    writeln(''),
    writeln("================================================================================"),
    writeln('                       | Digite as informações do jogo |'),
    writeln("================================================================================"),
    write('|Nome do jogo: '),
    read_line_to_string(user_input, NomeJogo),
    (
        gameAlreadyExistByName(Connection, NomeJogo) -> writeln("Nome de jogo ja existente, por favor, tente novamente."), cadastraJogo;
        write('|Descrição do jogo: '),
        read_line_to_string(user_input, DescricaoJogo),
        write('|Selecione o gênero do jogo: '),
        writeln('|1. Ação e Aventura'),
        writeln('|2. RPG '),
        writeln('|3. Terror'),
        writeln('|4. Estratégia'),
        writeln('|5. FPS'),
        read_line_to_string(user_input, Option),
        converteGenero(Option, GeneroJogo),
        write('|Valor do jogo: '),
        read_line_to_string(user_input, ValorDoJogoString),
        (
            not(verificaCamposNaoVazios([NomeJogo, DescricaoJogo, ValorDoJogoString])) -> 
            writeln("Você digitou um campo vazio, por favor, tente novamente."),
            cadastraJogo;
            atom_number(ValorDoJogoString, ValorDoJogo),
            (ValorDoJogo < 0 -> writeln("O valor do jogo não pode ser menor que zero, por favor, tente novamen"),
            cadastraJogo;
            /* Transformando a data atual para o formato YYYY-MM-DD */
            get_time(TStamp),
            format_time(string(Txt),'%FT%T%z',TStamp),
            split_string(Txt, "T", "", DataSplitada),
            nth0(0, DataSplitada, DataFormatada),
            Q = "INSERT INTO jogo (game_nome, game_genero, game_description, game_data_lancamento, game_avaliacao, game_price) values ('%w', '%w', '%w', '%w', %w, %w)",
            db_parameterized_query_no_return(Connection, Q, [NomeJogo, GeneroJogo, DescricaoJogo, DataFormatada, 0.0, ValorDoJogo]),
            writeln("Jogo cadastrado com sucesso"),
            encerrandoDatabase(Connection),
            menuAdmin)
    )).

converteGenero("1", "Ação e Aventura") :-!.
converteGenero("2", "RPG") :- !.
converteGenero("3", "Terror") :- !.
converteGenero("4", "Estratégia") :-!.
converteGenero("5", "FPS") :-!.
converteGenero(_, _) :- writeln("Opção inválida, tente novamente."), cadastraJogo, !.