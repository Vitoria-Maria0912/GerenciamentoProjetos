# Sistema de Gerenciamento de Projetos - PLP UFCG - 2023.1

                            ......   ......    ......   
                            .        .         .    .
                            ......   .  ...    ......        
                                 .   .    .    .
                            ......   ......    .
**Visão geral:**

Sistema que permitirá aos usuários gerenciar os seus projetos. Um projeto, além de envolver códigos, também envolve pessoas. Pelo sistema, será possível gerenciar tanto as tarefas que serão desenvolvidas, quanto as pessoas que fazem parte do projeto.



### Funcionalidades:

**1. Interface do usuário:**
   
  **_ Criação de perfil:**
  O usuário irá se cadastrar com seu nome e senha. O sistema retorna o ID do usuário.
     
  **_ Criação de projetos:**
  O usuário pode criar um projeto no sistema, informando o seu ID, nome do projeto, sua descrição, os membros que ele quer adicionar no projeto e as atividades. O sistema informa o ID do projeto criado.
  Quem cria o projeto, torna-se o gerente do mesmo.
         
  **_ Remoção de projetos:**
  Funcionalidade disponível apenas para gerentes de projeto. Para remoção, o usuário precisa informar o ID do projeto que deve ser removido.
    
  **_ Criação de atividades linkadas ao projeto - apenas o Gerente do projeto tem acesso:**
  O usuário pode criar atividades atreladas a um projeto, informando o título da atividade, sua descrição, o ID do membro responsável (um ou mais) e o sistema informa o ID da atividade.
  Essa funcionalidade está disponível apenas para gerentes de projeto.
    
  **_ Status da atividade:**
  Cada atividade pode ter três status distintos que mudam com base no inicio e fim da realização da atividade.
  Os status são: Não atribuída | Pendente | Concluída
      
 **_ Remoção de atividades:**
  Para remoção de uma atividade, o usuário informará o ID da atividade. 
  Apenas o gerente do projeto pode remover uma atividade do projeto.
    
  **_ Comentários em atividades:**
  Os usuários (membro atribuído a atividade ou gerente) têm a possibilidade de comentar insights/feedbacks sobre aquela atividade em específica durante a sua implementação.
    
  **_ Gerenciamento dos membros:**
  Há uma hierarquia, somente o usuário gerente pode acessar configurações do projeto.
    
  **_ Banco de atividades registradas:** 
  O sistema possuirá um banco com atividades - inicialmente com algumas atividades já inseridas na criação do sistema -, que crescerá com inclusão das atividades registradas pelos usuários.
  Com o banco de atividades, o gerente poderá selecionar um grupo de atividades, sem precisar sempre criar uma ‘nova’ atividade e atribuir a um grupo em um novo projeto. 
  As atividades podem ser classificadas em mais ou menos complexas e visualização em quais projetos elas já foram usadas. Isso ajudará o gerente na escolha de atividades para projetos que tenham características similares.
    
  **_ Caixa de mensagens:**
  Os usuários terão a possibilidade de interação através de uma caixa de mensagens. A caixa de mensagens terá a possibilidade de mensagens gerais (grupo do projeto) e mensagens privadas (de um usuário para outro do mesmo projeto). 

**2. Estrutura de dados:**
  Neste sistema os dados serão salvos em formato JSON.
   

 ### Pré-requesitos
 Você precisa ter a linguagens Haskell, o compilador ghc e o interpretador ghci em seu computador.

 ### Instalação
 adicionar passos da instalação necessária aqui

 ### Construído com
 - Haskell


 ### Autoras:
   - Íris Almeida - [Github] (https://github.com/irisalmeida)
   - Jamilly Venâncio - [Github] (https://github.com/venanciojamilly)
   - Vitória Maria - [Github] (https://github.com/Vitoria-Maria0912)
  -  Yalle Carvalho - [Github] (https://github.com/yallecarvalho)




