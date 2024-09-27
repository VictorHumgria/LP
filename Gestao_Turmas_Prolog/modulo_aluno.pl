:- module(aluno, [
  add_aluno/1,
  add_nota_aluno/3,
  exibir_aluno/1,
  exibir_notas_aluno/1,
  get_media_aluno/3,
  delete_aluno/1,
  load_banco_aluno/0,
  save_alunos/0
]).
:- use_module(library(csv)).
:- dynamic aluno/2.

% Estrutura aluno(Nome, [Turma, [Notas]]).
% NotasPorTurma é uma lista de turma-notas, ex: [('TurmaA', [7.5, 8.0]), ('TurmaB', [9.0])].
load_banco_aluno :-
	(
		exists_file('alunos.csv')
		 ->
				consult('alunos.csv'), 
		write('Dados dos alunos carregados com sucesso.'), nl;
		write('Nenhum arquivo de alunos encontrado.'), nl).
save_alunos :-
	tell('alunos.csv'), 
	listing(aluno/2), told.
add_aluno(Nome) :-
	 \+ aluno(Nome, _), 
	assertz(
		aluno(Nome, [])), save_alunos, 
	write('Aluno cadastrado com sucesso.'), nl.
add_nota_aluno(Nome, Turma, Nota) :-
	aluno(Nome, NotasPorTurma), 
	(
		select(
			(Turma, NotasTurma), NotasPorTurma, RestNotasPorTurma)
		 ->
				append(NotasTurma, [Nota], NovasNotasTurma), 
		NovoNotasPorTurma = [(Turma, NovasNotasTurma)|RestNotasPorTurma];
		NovoNotasPorTurma = [(Turma, [Nota])|NotasPorTurma]), 
	retract(
		aluno(Nome, NotasPorTurma)), 
	assertz(
		aluno(Nome, NovoNotasPorTurma)), save_alunos, 
	write('Nota adicionada com sucesso.'), nl.
exibir_aluno(Nome) :-
	aluno(Nome, NotasPorTurma), 
	write('Nome: '), 
	write(Nome), nl, 
	write('Notas por Turma: '), nl, 
	exibir_notas_por_turma(NotasPorTurma).

exibir_notas_por_turma([]).
exibir_notas_por_turma([(Turma, Notas)|Rest]) :-
	write('  '), 
	write(Turma), 
	write(': '), 
	write(Notas), nl, 
	exibir_notas_por_turma(Rest).
exibir_notas_aluno(Nome) :-
	aluno(Nome, NotasPorTurma), 
	write('Notas de '), 
	write(Nome), 
	write(':'), nl, 
	exibir_notas_por_turma(NotasPorTurma).
get_media_aluno(Nome, Turma, Media) :-
	aluno(Nome, NotasPorTurma), 
	member(
		(Turma, Notas), NotasPorTurma), 
	calcular_media(Notas, Media), 
	write('A média de '), 
	write(Nome), 
	write(' na turma '), 
	write(Turma), 
	write(' é: '), 
	write(Media), nl.
calcular_media(Notas, Media) :-
	sum_list(Notas, Soma), 
	length(Notas, Quantidade), Quantidade > 0, Media is Soma/Quantidade.
delete_aluno(Nome) :-
	aluno(Nome, _), 
	retract(
		aluno(Nome, _)), save_alunos, 
	write('Aluno removido do sistema'), nl.