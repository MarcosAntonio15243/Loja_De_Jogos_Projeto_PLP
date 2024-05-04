# :video_game:🕹️ Loja de Jogos  

Neste projeto, criaremos um sistema de uma loja de jogos digitais, possibilitando ao usuário uma forma fácil de adquirir seus jogos e mantê-los em sua biblioteca, gerenciar sua carteira digital e diversas outras funcionalidades utilizando apenas o terminal de linha de comando.  

Cada usuário possuirá uma conta, podendo esta ser administrador (admin) ou padrão. Assim, após a criação da conta, o usuário padrão poderá realizar login no sistema para visualizar os jogos, avaliá-los, deixar comentários sobre o jogo, comprar os jogos que desejar, gerenciar sua carteira digital com operações de depósito e consulta de saldo, e também bater um papo com outro usuário através de um chat privado.  

Para facilitar a busca de jogos por parte do usuário, além da pesquisa por nome, o sistema conta com métodos de filtragem para exibição dos jogos (mais vendidos, mais jogados, lançamentos, gêneros como RPG, terror, puzzle, etc).  

O usuário administrador tem privilégios no sistema, como cadastrar jogos, atualizar jogos, ter um dashboard com diversas estatísticas do sistema e analisar as denúncias feitas pelos usuários.  

# :gear::clipboard: Funcionalidades  
- **Gerais**
  - Criar conta;
  - Fazer login.
 
- **Administrador**
  - Adicionar, atualizar, visualizar e remover jogos;
  - Analisar denúncias;
  - Visualizar dashboard com estatísticas do sistema.

- **Usuário padrão**
  - Visualizar jogos disponíveis (A visualização pode ser filtrada por gênero, mais vendidos, data de lançamento, mais avaliados, nome e preço);
  - Comprar jogos disponíveis;
  - Avaliar jogos;
  - Favoritar jogos;
  - Gerenciar carteira digital (depósito e consulta de saldo);
  - Comentar jogo;
  - Denunciar jogo;
  - Conversar através de chat privado com outros usuários.

  

# :wrench::computer: Como executar  
## Instalação do PostgreSQL
1. Baixe o PostgreSQL:
   
   - [Tutorial para Windows](https://www.youtube.com/watch?v=L_2l8XTCPAE&list=LL&index=5)
   - [Tutorial para Linux Ubuntu](https://www.youtube.com/watch?v=1jSb4LJH1dw)
   - [Tutorial para Linux Mint (Em caso de problemas no vídeo acima)](https://www.youtube.com/watch?v=rDh3iq8nmDg)
  
2. Se for executar o projeto em Prolog, precisará baixar o driver do PostgreSQL OBDC para Windows ([Link](https://www.postgresql.org/ftp/odbc/versions/msi/))
     - Após isso, você cria a database "lojajogos" no pgAdmin
     - Pesquise no menu iniciar por ODBC e clique na primeira opção. A tela “Administrador de Fonte de Dados ODBC” deve abrir. Na aba “DNS de Usuário” clique em Adicionar, e crie uma fonte de dados com o driver “PostgreSQL Unicode” que você baixou anteriormente. Os dados a serem inseridos são dessa forma (semelhante à anterior):
       
       ```
        Data Source: SWI-Prolog 
        Database: lojajogos
        Server: localhost
        User: postgres
        Description: Conexão ODBC para o PostgreSQL
        SSL mode: disable 
        Port: 5432 
        Password: postgres 
       ```
     
4. Após a instalação, entre no pgAdmin e crie a database com o nome "lojajogos"
5. Configurações iniciais:
   - Caso peça para configurar username e password antes do passo 2, configure da seguinte forma:
     
      ```
      username: postgres
      password: postgres
      host name/address: localhost
      port: 5432
      ```
   - Caso não peça, clique com o botão direito no servidor onde está sua database, vá em connection e configure com as informações acima.
4. Após as configurações, execute o projeto e em seguida copie o conteúdo do arquivo [dadosIniciais.sql](https://github.com/MarcosAntonio15243/Loja_De_Jogos_Projeto_PLP/blob/main/haskell/dadosIniciais.sql), cole na Query Tool do pgAdmin e execute, um comando por vez, para popular uma conta administrador padrão e alguns jogos na sua base de dados.
       
___Observação: A configuração deve seguir a mesma que o tutorial acima. Caso contrário, a aplicação pode resultar em erros.___  

## Instalação do Haskell
1. Faça a instalação do [GHCup](https://www.haskell.org/ghcup/)
2. Depois de configurar seu PostgreSQL, abra o terminal no diretório "haskell" e rode os comandos `cabal init -n` e `cabal build`
3. Caso não ocorra erros, digite `cabal run`

___Observação 1: Caso esteja usando Visual Studio Code, baixe as extensões necessárias.___  
___Observação 2: Em caso de erro na lib "base", você pode alterar a versão manualmente no arquivo "haskell.cabal" com a versão da sua máquina.___

## Instalação do Prolog
1. Faça a instalação do [SWI-Prolog](https://www.swi-prolog.org/download/stable) ([Tutorial de instalação para Windows](https://www.youtube.com/watch?v=YzDpQOk2qvQ&t=11s))
2. Depois de configurar seu PostgreSQL, abra o terminal no diretório "prolog" e rode os comandos `swipl main.pl` e em seguida chame a função `main.`
   
# 👨‍💻👩‍💻 Equipe  

- [Marcos Vinícius](https://github.com/marcosfragoso)
- [Marcos Antônio](https://github.com/MarcosAntonio15243)
- [Leila Farias](https://github.com/LeilaFarias)
- [Hildon Regis](https://github.com/Hildon27)
- [João Victor](https://github.com/VictorCosme)

