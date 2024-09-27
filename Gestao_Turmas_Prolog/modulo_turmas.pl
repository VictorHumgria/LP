:- module(turmas, [
  load_banco/0,
  add_turma_input/0,
  cadastra_aluno_turma/2,
  get_turmas/0,
  exibir_alunos/1,
  listar_turmas_aluno/1,
  del_turma/0,
  del_aluno/0,
  remove_aluno_all_turmas/1,
  turma/4
  ]).
:- dynamic(turma/4).
:- use_module('modulo_aluno.pl').
:- use_module(library(csv)).

save_turmas :-
    tell('turmas.csv'),  % abre o arquivo para escrita
    listing(turma/4),
  % ele basicamente exibe o conteudo dos predicados na base de local, como por exemplo quando usando add_turma_input que tem comportamenteo dinamicoe fica armazenado dinamicamente na execução do codigo ai ao final ele "imprime" como se fosse o write no arquivo, mas como usei tell ele ira imprimir no arquivo.
    told.
% busca a turma do banco de dados.
load_banco:- %carregar as turmas no arquivo csv
    (exists_file('turmas.csv')->  %funcao nativa do swi-prolog - verifica se o arquivo existe.
    consult('turmas.csv'),        %funcao nativa do prolog onde carregar uma base de dados dinamica.
    write('Turmas carregadas com sucesso'),nl,! ;
    write('Nenhum arquivo de turmas encontrado'),nl).


add_turma_input :-
    write('Digite o nome da nova turma: '), nl,
    read(NomeTurma),
    write('Digite o código da nova turma: '), nl,
    read(CodigoTurma),
    write('Digite o nome do professor(a): '), nl,
    read(Professor),
    assertz(turma(NomeTurma, CodigoTurma, Professor,_)),
    save_turmas,
    write('Nova turma adicionada com sucesso!'), nl,!.

% Predicado para listar todas as turmas e suas propriedades
get_turmas :-
    load_banco,
    turma(Nome,Codigo,Professor, Alunos),
    write('------------------------------------'),nl,
    write('Turma: '), write(Nome), nl,
    write('Codigo: '), write(Codigo), nl,
    write('Professor(a): '), write(Professor), nl,
    write('Alunos: '), write(Alunos), nl,nl,
    fail.  % Força o backtracking para listar todas as turmas

% Predicado para adicionar um aluno a uma turma
cadastra_aluno_turma(NomeTurma, Aluno) :-
  turma(NomeTurma, Codigo, Professor, AlunosAtuais),
  retract(turma(NomeTurma, Codigo, Professor, AlunosAtuais)),
  assertz(turma(NomeTurma, Codigo, Professor, [Aluno|AlunosAtuais])),
  save_turmas,
  % Adicionar a turma ao cadastro do aluno sem nota inicial
  aluno:add_nota_aluno(Aluno, NomeTurma, []),
  write('Aluno adicionado ? turma com sucesso.'), nl.

% Função recursiva para extrair os nomes dos alunos
extrair_nomes_alunos([], []).  % Caso base: lista vazia
extrair_nomes_alunos([Nome | Calda], [Nome | NomesRestantes]) :- % se for usar mais elementos no tipo aluno é preciso usar variavel anonima para conseguir usar a funcao.
    extrair_nomes_alunos(Calda, NomesRestantes).  % Chamada recursiva para o restante da lista


% Exibe apenas o nome dos alunos usando a função acima
exibir_alunos(NomeTurma):-
    load_banco,
    turma(NomeTurma,_,_,Alunos),
    extrair_nomes_alunos(Alunos,NomesAlunos),
    write('Turma: '),nl, write(NomeTurma),nl,write('Alunos: '),write(NomesAlunos),!,nl.

%funcao Recursiva verifica Aluno

% Função recursiva para verificar em quais turmas o aluno está matriculado
turmas_aluno(_, [], []).  % Caso base: não há mais turmas para verificar
turmas_aluno(Aluno, [turma(NomeTurma, _, _, Alunos) | RestoTurmas], [NomeTurma | OutrasTurmas]) :-
    member(Aluno, Alunos),  % Se o aluno está matriculado nesta turma
    turmas_aluno(Aluno, RestoTurmas, OutrasTurmas).  % Chamada recursiva para o restante das turmas
turmas_aluno(Aluno, [_ | RestoTurmas], OutrasTurmas) :-  % Se o aluno não está matriculado nesta turma
    turmas_aluno(Aluno, RestoTurmas, OutrasTurmas).  % Continuar buscando nas outras turmas

% Predicado para listar todas as turmas (manualmente)
listar_turmas(Turmas) :-
    findall(turma(Nome, Codigo, Professor, Alunos), turma(Nome, Codigo, Professor, Alunos), Turmas).

% Predicado para listar as turmas de um aluno
listar_turmas_aluno(Aluno) :-
    listar_turmas(TodasTurmas),  % Coleta todas as turmas da base de dados
    turmas_aluno(Aluno, TodasTurmas, TurmasDoAluno),  % Busca em quais turmas o aluno está
    (TurmasDoAluno = [] ->
        write(Aluno), write(' não está matriculado em nenhuma turma.'), nl;
        write('O aluno '), write(Aluno), write(' está matriculado nas turmas: '), nl,
        print_turmas(TurmasDoAluno)),!.

% Função auxiliar para exibir as turmas
print_turmas([]) :- !.  % Caso base: não há mais turmas para exibir
print_turmas([Turma | Resto]) :-
   write(Turma), write('-'),
    print_turmas(Resto).

del_turma:-
    write('Digite o codigo que da turma a ser excluida: '),nl,
    read(Id),
    turma(Nome, Id, Professor, Alunos) ->
        retract(turma(Nome, Id, Professor, Alunos)),
     save_turmas,
    write('Turma excluida e novo arquivo salvo.'),nl,!.

del_aluno:-
    write('Digite o nome da turma do aluno: '),nl,
    read(NomeTurma),
    write('Digite o nome do aluno: '),nl,
    read(NomeAluno),
    (turma(NomeTurma,Codigo,Professor,AlunosAtuais),
     member(NomeAluno, AlunoAtuais) ->
    delete(AlunosAtuais, NomeAluno, AlunosAtualizados),
     retract(turma(NomeTurma, Codigo, Professor,AlunoAtuais)),
     assertz(turma(NomeTurma, Codigo, Professor,AlunosAtualizados)),
     save_turmas,
     write('Aluno removido'),nl).
    
remove_aluno_all_turmas(Aluno) :-
  findall(turma(Nome, Codigo, Professor, Alunos),
          turma(Nome, Codigo, Professor, Alunos),
          TodasTurmas),
  remove_aluno_from_turmas(Aluno, TodasTurmas),
  save_turmas,
  write('Aluno removido de todas as turmas.'), nl.
  
remove_aluno_from_turmas(_, []).
remove_aluno_from_turmas(Aluno, [turma(Nome, Codigo, Professor, Alunos)|Rest]) :-
  delete(Alunos, Aluno, NovosAlunos),
  retract(turma(Nome, Codigo, Professor, Alunos)),
  assertz(turma(Nome, Codigo, Professor, NovosAlunos)),
  remove_aluno_from_turmas(Aluno, Rest).