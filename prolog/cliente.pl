:- module(cliente, [perfilCliente/2]).
:- use_module(util).

perfilCliente(Connection, UserID) :-
    getNickByID(Connection, UserID, Nickname),
    
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

            read(Opcao),
            limparTela, 
            
            (   Opcao =:= 1 -> jogosCliente(Connection, UserID);
                Opcao =:= 2 -> carteiraCliente(Connection, UserID);
                Opcao =:= 3 -> editarPerfil(Connection, UserID);
                Opcao =:= 4 -> menuCliente(Connection, UserID);
                writeln('Opção inválida! Por favor, tente novamente.'),
                perfilCliente(Connection, UserID)
            )
    ;   writeln('ID Usuário Inválido')
    ).

% Obtém o nickname de cada usuário a partir do Banco de Dados. Por ora, não foi implementado o acesso.
getNickByID(Connection, UserID, Nickname) :-
    (   UserID =:= 123 -> Nickname = 'Alice';
        Nickname = 'invalid_user_id'
    ).

jogosCliente(Connection, UserID) :-
    % É preciso implementar o predicado que recupera os jogos do usuário a partir do BD. 
    Jogos = [123, 245],
    
    writeln("================================================================================"),
    writeln("                                Meus Jogos                                      "),
    writeln("================================================================================"),
    exibirJogosCliente(Jogos),
    
    nl,
    
    writeln('Digite "sair" para voltar'),
    writeln('Insira o ID do jogo que deseja acessar:'),
    read(Input),
    limparTela,
    
    (   Input == sair -> perfilCliente(Connection, UserID);
        (
            member(Input, Jogos) -> acessarJogo(Connection, UserID, Input);
            writeln('ID do jogo inválido.'),
            jogosCliente(Connection, UserID)
        )
    ).

acessarJogo(Connection, UserID, JogoID) :-
    writeln('Opções disponíveis para o jogo:'),
    writeln('1. Avaliar o jogo'),
    writeln('2. Favoritar o jogo'),
    writeln('3. Deixar um comentário'),
    writeln('4. Denunciar o jogo'),
    writeln('5. Voltar'),
    writeln('Digite o número da opção desejada:'),

    read(Opcao),
    limparTela,
    
    (   Opcao =:= 1 -> avaliarJogo(Connection, UserID, JogoID);
        Opcao =:= 2 -> favoritarJogo(Connection, UserID, JogoID);
        Opcao =:= 3 -> comentarJogo(Connection, UserID, JogoID);
        Opcao =:= 4 -> denunciarJogo(Connection, JogoID, UserID);
        Opcao =:= 5 -> jogosCliente(Connection, UserID);
        writeln('Opção inválida.')
    ),

    acessarJogo(Connection, UserID, JogoID).

avaliarJogo(Connection, UserID, JogoID) :-
   
    writeln('================================================================================'),
    writeln('Insira uma nota de 0 a 10 > '),

    read(Nota),
    (   number(Nota),
        0 =< Nota,
        Nota =< 10 
        
        -> (    registrarAvaliacao(Connection, JogoID, UserID, Nota),
                limparTela,
                writeln('================================================================================'),
                writeln('                         Jogo avaliado com sucesso                              '),
                writeln('================================================================================')
            )
        
        ;  (
                limparTela,
                writeln('Entrada inválida. Por favor, insira um valor dentro da faixa'),
                avaliarJogo(Connection, UserID, JogoID)
            )
    ).

favoritarJogo(Connection, UserID, JogoID) :-
    % Supus que exista fora de cliente.pl. Atentar também para o predicado 'execute' que é chamado dentro da função.
    getFavoritarJogo(Connection, JogoID, UserID, EstaFavoritado),

    (   EstaFavoritado ->
        writeln('================================================================================'),
        writeln('O jogo já está em seus favoritos'),
        writeln('Deseja removê-lo dos favoritos? (s/n) >'),

        read_line_to_string(user_input, Opcao),
        limparTela,

        (   string_lower(Opcao, 's') ->
            
            execute(Connection, 'UPDATE compra SET favoritar_jogo = false WHERE game_id = ? and user_id = ?', [JogoID, UserID]),
            writeln('================================================================================'),
            writeln('Jogo removido dos favoritos!')
        ;   string_lower(Opcao, 'n') ->
            true
        ;   writeln('Opção inválida!')
        )
    ;   execute(Connection, 'UPDATE compra SET favoritar_jogo = true WHERE game_id = ? and user_id = ?', [JogoID, UserID]),
        writeln('================================================================================'),
        writeln('Jogo favoritado com sucesso')
    ).

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
exibirJogosCliente([ID|Resto]) :-
    writeln(ID),
    exibirJogosCliente(Resto).
