:- module(cliente, [perfilCliente/2]).
:- use_module(util).

% Predicado que exibe as opções do perfil
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
            );

        writeln('ID Usuário Inválido')
    ).

% Obtém o nickname de cada usuário a partir do Banco de Dados. Por ora, não foi implementado o acesso.
getNickByID(Connection, UserID, Nickname) :-
    (   UserID =:= 123 -> Nickname = 'Alice';
        Nickname = 'invalid_user_id'
    ).

jogosCliente(Connection, UserID) :-
    writeln('Exibindo jogos do cliente...').

carteiraCliente(Connection, UserID) :-
    writeln('Exibindo carteira do cliente...').

editarPerfil(Connection, UserID) :-
    writeln('Editando perfil do cliente...').

menuCliente(Connection, UserID) :-
    writeln('Voltando ao menu principal...').