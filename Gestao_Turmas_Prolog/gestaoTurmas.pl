:-initialization(main).
:-use_module(['modulo_turmas.pl', 'modulo_aluno.pl']).
main :-
	on_signal(int,_,fechar_programa),
	write('Sistema de Gestao de Turmas e Alunos'), nl, turmas : load_banco, aluno : load_banco_aluno,
	write('-------------------------------'), nl, menu.
menu :-
	write('1. Cadastrar Turma'), nl,
	write('2. Cadastrar Aluno'), nl,
	write('3. Adicionar Aluno a Turma'), nl,
	write('4. Listar Turmas'), nl,
	write('5. Listar Alunos de uma Turma'), nl,
	write('6. Listar Turmas de um Aluno'), nl,
	write('7. Excluir Turma'), nl,
	write('8. Excluir Aluno de uma Turma'), nl,
	write('9. Adicionar Nota a um Aluno'), nl,
	write('10. Exibir Notas de um Aluno'), nl,
	write('11. Calcular Media de um Aluno'), nl,
	write('12. Excluir Aluno do Sistema'), nl,
	write('13. Sair'), nl,
	read(Choice),
	guia_escolha(Choice).
guia_escolha(1) :-
	cadastrar_turma.
guia_escolha(2) :-
	cadastrar_aluno.
guia_escolha(3) :-
	adicionar_aluno_turma.
guia_escolha(4) :-
	listar_turmas.
guia_escolha(5) :-
	listar_alunos_turma.
guia_escolha(6) :-
	listar_turmas_aluno.
guia_escolha(7) :-
	excluir_turma.
guia_escolha(8) :-
	excluir_aluno_turma.
guia_escolha(9) :-
	adicionar_nota_aluno.
guia_escolha(10) :-
	exibir_notas_aluno.
guia_escolha(11) :-
	calcular_media_aluno.
guia_escolha(12) :-
	excluir_aluno_sistema.
guia_escolha(13) :-
	write('Saindo do sistema...'), nl, fechar_programa.
guia_escolha(_) :-
	write('Opcao invalida. Tente novamente.'), nl, menu.
cadastrar_turma :-
	write('Cadastrar Turma'), nl,
	write('---------------'), nl, turmas : add_turma_input, nl, menu.
cadastrar_aluno :-
	write('Cadastrar Aluno'), nl,
	write('--------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(Nome),
	aluno : add_aluno(Nome), nl, menu.
adicionar_aluno_turma :-
	write('Adicionar Aluno a Turma'), nl,
	write('------------------------'), nl,
	write('Digite o nome da turma: '), nl,
	read(NomeTurma),
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	turmas : cadastra_aluno_turma(NomeTurma, NomeAluno), nl, menu.
listar_turmas :-
	write('Listar Turmas'), nl,
	write('--------------'), nl, turmas : get_turmas, menu.
listar_alunos_turma :-
	write('Listar Alunos de uma Turma'), nl,
	write('---------------------------'), nl,
	write('Digite o nome da turma: '), nl,
	read(NomeTurma),
	turmas : exibir_alunos(NomeTurma), nl, menu.
listar_turmas_aluno :-
	write('Listar Turmas de um Aluno'), nl,
	write('--------------------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	turmas : listar_turmas_aluno(NomeAluno), nl, menu.
excluir_turma :-
	write('Excluir Turma'), nl,
	write('--------------'), nl, turmas : del_turma, nl, menu.
excluir_aluno_turma :-
	write('Excluir Aluno de uma Turma'), nl,
	write('----------------------------'), nl, turmas : del_aluno, nl, menu.
adicionar_nota_aluno :-
	write('Adicionar Nota a um Aluno'), nl,
	write('--------------------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	write('Digite o nome da turma: '), nl,
	read(NomeTurma),
	write('Digite a nota: '), nl,
	read(Nota),
	aluno : add_nota_aluno(NomeAluno, NomeTurma, Nota), nl, menu.
exibir_notas_aluno :-
	write('Exibir Notas de um Aluno'), nl,
	write('-------------------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	aluno : exibir_notas_aluno(NomeAluno), nl, menu.
calcular_media_aluno :-
	write('Calcular Media de um Aluno'), nl,
	write('---------------------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	write('Digite o nome da turma: '), nl,
	read(NomeTurma),
	aluno : get_media_aluno(NomeAluno, NomeTurma, _), nl, menu.
excluir_aluno_sistema :-
	write('Excluir Aluno do Sistema'), nl,
	write('--------------------------'), nl,
	write('Digite o nome do aluno: '), nl,
	read(NomeAluno),
	turmas :  remove_aluno_all_turmas(NomeAluno),
	aluno : delete_aluno(NomeAluno), nl, menu.
