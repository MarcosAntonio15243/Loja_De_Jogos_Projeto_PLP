INSERT INTO jogo (game_nome, game_genero, game_description, game_data_lancamento, game_avaliacao, game_price) 
VALUES 
('The Witcher 3: Wild Hunt', 'RPG de ação', 'Um RPG de ação épico ambientado em um mundo de fantasia aberto.', '2015-05-19', 9, 49.99),
('Grand Theft Auto V', 'Ação e aventura', 'Um jogo de mundo aberto que permite aos jogadores explorar uma recriação fictícia de Los Angeles.', '2013-09-17', 8, 29.99),
('Red Dead Redemption 2', 'Ação e aventura', 'Um épico de ação e aventura no Velho Oeste.', '2018-10-26', 9, 59.99),
('The Last of Us Part II', 'Ação e aventura', 'Uma aventura emocional em um mundo pós-apocalíptico.', '2020-06-19', 9, 69.99),
('Dark Souls III', 'RPG de ação', 'Um RPG de ação desafiador ambientado em um mundo de fantasia sombrio.', '2016-04-12', 9, 39.99),
('Horizon Zero Dawn', 'RPG de ação', 'Um RPG de ação ambientado em um mundo pós-apocalíptico dominado por máquinas.', '2017-02-28', 8, 19.99),
('Final Fantasy VII Remake', 'RPG', 'Um remake do clássico jogo de RPG japonês.', '2020-04-10', 9, 59.99),
('Assassin''s Creed Valhalla', 'Ação e aventura', 'Um jogo de ação e aventura que se passa na era dos vikings.', '2020-11-10', 8, 49.99),
('Cyberpunk 2077', 'RPG de ação', 'Um RPG de mundo aberto ambientado em um futuro distópico.', '2020-12-10', 7, 59.99),
('Ghost of Tsushima', 'Ação e aventura', 'Um jogo de ação e aventura situado no Japão feudal.', '2020-07-17', 8, 49.99),
('Bloodborne', 'RPG de ação', 'Um RPG de ação desafiador ambientado em um mundo de terror gótico.', '2015-03-24', 9, 39.99),
('Marvel''s Spider-Man', 'Ação e aventura', 'Um jogo de ação e aventura baseado no popular super-herói da Marvel.', '2018-09-07', 8, 39.99),
('God of War', 'Ação e aventura', 'Um jogo de ação e aventura baseado na mitologia grega.', '2018-04-20', 9, 39.99);


INSERT INTO usuario (user_nome, user_email, user_senha, user_tipo, user_saldo, user_date) VALUES
    ('Administrador', 'admin@gmail.com', 'admin', 'Admin', 100, '2024-03-11'),
    ('João Silva', 'joao@example.com', '123456', 'Padrão',100, '2024-03-11'),
    ('Maria Santos', 'maria@example.com', 'password123', 'Padrão',100, '2024-03-11'),
    ('Carlos Oliveira', 'carlos@example.com', 'abcdef', 'Padrão',100, '2024-03-11');


INSERT INTO compra (compra_data, compra_price, user_id, game_id) VALUES
    ('2024-03-11', 49.99, 2, 1),  -- Compra 1: João Silva comprou The Witcher 3: Wild Hunt
    ('2024-03-11', 29.99, 3, 2),  -- Compra 2: Maria Santos comprou Grand Theft Auto V
    ('2024-03-11', 59.99, 4, 3),  -- Compra 3: Carlos Oliveira comprou Red Dead Redemption 2
    ('2024-03-11', 69.99, 2, 4),  -- Compra 4: João Silva comprou The Last of Us Part II
    ('2024-03-11', 39.99, 3, 5),  -- Compra 5: Maria Santos comprou Dark Souls III
    ('2024-03-11', 19.99, 4, 6),  -- Compra 6: Carlos Oliveira comprou Horizon Zero Dawn
    ('2024-03-11', 59.99, 2, 7),  -- Compra 7: João Silva comprou Final Fantasy VII Remake
    ('2024-03-11', 49.99, 3, 8),  -- Compra 8: Maria Santos comprou Assassin's Creed Valhalla
    ('2024-03-11', 59.99, 4, 9),  -- Compra 9: Carlos Oliveira comprou Cyberpunk 2077
    ('2024-03-11', 49.99, 2, 10); -- Compra 10: João Silva comprou Ghost of Tsushima