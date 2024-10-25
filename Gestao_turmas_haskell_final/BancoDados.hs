module BancoDados (
    aluno01, aluno02, aluno03, aluno04, aluno05, aluno06, aluno07, aluno08, aluno09, aluno10,
    aluno11, aluno12, aluno13, aluno14, aluno15, aluno16, aluno17, aluno18, aluno19, aluno20,
    aluno21, aluno22, aluno23, aluno24, aluno25, aluno26, aluno27, aluno28, aluno29, aluno30,
    aluno31, aluno32, aluno33, turma01, turma02, turma03, turma04, turma05, turma06, turma07,
    turma08, turma09, turma11, turma12, turma13, turma14, turma15, turma16, turma17, turma18,
    turma19, turma20, turma21, turma22, turma23, turma24, turma25, turmas
) where

import Alunos
import Turmas

aluno01 = cadastraAluno (criaPessoa "Joao" "123456789" "1990-01-01") 123 True "2022-01-01" "joao@example.com" [5.1,5.4,90.0]
aluno02 = cadastraAluno (criaPessoa "Maria" "987654321" "1991-02-02") 124 True "2022-02-02" "maria@example.com" [4.2,3.5,80.0]
aluno03 = cadastraAluno (criaPessoa "Jose" "111111111" "1992-03-03") 125 True "2022-03-03" "jose@example.com" [3.8,4.1,85.0]
aluno04 = cadastraAluno (criaPessoa "Ana" "222222222" "1993-04-04") 126 True "2022-04-04" "ana@example.com" [4.5,3.9,90.0]
aluno05 = cadastraAluno (criaPessoa "Pedro" "333333333" "1994-05-05") 127 True "2022-05-05" "pedro@example.com" [4.8,4.2,95.0]
aluno06 = cadastraAluno (criaPessoa "Luisa" "444444444" "1995-06-06") 128 True "2022-06-06" "luisa@example.com" [4.1,3.7,80.0]
aluno07 = cadastraAluno (criaPessoa "Carlos" "555555555" "1996-07-07") 129 True "2022-07-07" "carlos@example.com" [4.3,4.5,92.0]
aluno08 = cadastraAluno (criaPessoa "Beatriz" "666666666" "1997-08-08") 130 True "2022-08-08" "beatriz@example.com" [4.6,4.1,93.0]
aluno09 = cadastraAluno (criaPessoa "Rafael" "777777777" "1998-09-09") 131 True "2022-09-09" "rafael@example.com" [4.9,4.3,96.0]
aluno10 = cadastraAluno (criaPessoa "Gabriela" "888888888" "1999-10-10") 132 True "2022-10-10" "gabriela@example.com" [4.7,4.2,94.0]
aluno11 = cadastraAluno (criaPessoa "Felipe" "999999999" "2000-11-11") 133 True "2022-11-11" "felipe@example.com" [4.4,4.6,91.0]
aluno12 = cadastraAluno (criaPessoa "Julia" "101010101" "2001-12-12") 134 True "2022-12-12" "julia@example.com" [4.5,4.8,95.0]
aluno13 = cadastraAluno (criaPessoa "Vinicius" "121212121" "2002-01-01") 135 True "2023-01-01" "vinicius@example.com" [4.9,4.7,97.0]
aluno14 = cadastraAluno (criaPessoa "Leticia" "131313131" "2003-02-02") 136 True "2023-02-02" "leticia@example.com" [4.6,4.4,92.0]
aluno15 = cadastraAluno (criaPessoa "Matheus" "141414141" "2004-03-03") 137 True "2023-03-03" "matheus@example.com" [4.8,4.9,98.0]
aluno16 = cadastraAluno (criaPessoa "Amanda" "151515151" "2005-04-04") 138 True "2023-04-04" "amanda@example.com" [4.7,4.5,93.0]
aluno17 = cadastraAluno (criaPessoa "Thiago" "161616161" "2006-05-05") 139 True "2023-05-05" "thiago@example.com" [4.9,4.8,99.0]
aluno18 = cadastraAluno (criaPessoa "Joao" "123456789" "1990-01-01") 101 True "2022-01-01" "joao@example.com" [5.1,5.4,90.0]
aluno19 = cadastraAluno (criaPessoa "Maria" "987654321" "1991-02-02") 102 True "2022-02-02" "maria@example.com" [4.2,3.5,80.0]
aluno20 = cadastraAluno (criaPessoa "Jose" "111111111" "1992-03-03") 103 True "2022-03-03" "jose@example.com" [3.8,4.1,85.0]
aluno21 = cadastraAluno (criaPessoa "Ana" "222222222" "1993-04-04") 104 True "2022-04-04" "ana@example.com" [4.5,3.9,90.0]
aluno22 = cadastraAluno (criaPessoa "Pedro" "333333333" "1994-05-05") 105 True "2022-05-05" "pedro@example.com" [4.8,4.2,95.0]
aluno23 = cadastraAluno (criaPessoa "Luisa" "444444444" "1995-06-06") 106 True "2022-06-06" "luisa@example.com" [4.1,3.7,80.0]
aluno24 = cadastraAluno (criaPessoa "Carlos" "555555555" "1996-07-07") 107 True "2022-07-07" "carlos@example.com" [4.3,4.5,92.0]
aluno25 = cadastraAluno (criaPessoa "Beatriz" "666666666" "1997-08-08") 108 True "2022-08-08" "beatriz@example.com" [4.6,4.1,93.0]
aluno26 = cadastraAluno (criaPessoa "Rafael" "777777777" "1998-09-09") 109 True "2022-09-09" "rafael@example.com" [4.9,4.3,96.0]
aluno27 = cadastraAluno (criaPessoa "Gabriela" "888888888" "1999-10-10") 110 True "2022-10-10" "gabriela@example.com" [4.7,4.2,94.0]
aluno28 = cadastraAluno (criaPessoa "Felipe" "999999999" "2000-11-11") 111 True "2022-11-11" "felipe@example.com" [4.4,4.6,91.0]
aluno29 = cadastraAluno (criaPessoa "Julia" "101010101" "2001-12-12") 112 True "2022-12-12" "julia@example.com" [4.5,4.8,95.0]
aluno30 = cadastraAluno (criaPessoa "Vinicius" "121212121" "2002-01-01") 113 True "2023-01-01" "vinicius@example.com" [4.9,4.7,97.0]
aluno31 = cadastraAluno (criaPessoa "Leticia" "131313131" "2003-02-02") 114 True "2023-02-02" "leticia@example.com" [4.6,4.4,92.0]
aluno32 = cadastraAluno (criaPessoa "Matheus" "141414141" "2004-03-03") 115 True "2023-03-03" "matheus@example.com" [4.8,4.9,98.0]
aluno33 = cadastraAluno (criaPessoa "Amanda" "151515151" "2005-04-04") 116 True "2023-04-04" "amanda@example.com" [4.7,4.5,93.0]

--Turmas

turma01 = cadastraTurma "Turma 01" 01 "fulano" "2023.2"
turma02 = cadastraTurma "Turma 02" 02 "beltrano" "2023.1"
turma03 = cadastraTurma "Turma 03" 03 "sicrano" "2022.2"
turma04 = cadastraTurma "Turma 04" 04 "aluno1" "2022.1"
turma05 = cadastraTurma "Turma 05" 05 "professorX" "2024.1"
turma06 = cadastraTurma "Turma 06" 06 "aluno2" "2024.2"
turma07 = cadastraTurma "Turma 07" 07 "fulana" "2023.1"
turma08 = cadastraTurma "Turma 08" 08 "beltrana" "2022.2"
turma09 = cadastraTurma "Turma 09" 09 "sicrana" "2024.1"
turma11 = cadastraTurma "Turma 10" 10 "aluno3" "2023.2"
turma12 = cadastraTurma "Turma 12" 11 "aluno3" "2023.2"
turma13 = cadastraTurma "Turma 13" 12 "aluno3" "2023.2"
turma14 = cadastraTurma "Turma 14" 13 "aluno3" "2023.2"
turma15 = cadastraTurma "Turma 15" 14 "aluno3" "2023.2"
turma16 = cadastraTurma "Turma 16" 15 "aluno3" "2023.2"
turma17 = cadastraTurma "Turma 17" 16 "aluno3" "2023.2"
turma18 = cadastraTurma "Turma 18" 17 "aluno3" "2023.2"
turma19 = cadastraTurma "Turma 19" 18 "aluno3" "2023.2"
turma20 = cadastraTurma "Turma 20" 19 "aluno3" "2023.2"
turma21 = cadastraTurma "Turma 21" 20 "aluno3" "2023.2"
turma22 = cadastraTurma "Turma 22" 21 "aluno3" "2023.2"
turma23 = cadastraTurma "Turma 23" 22 "aluno3" "2023.2"
turma24 = cadastraTurma "Turma 24" 23 "aluno3" "2023.2"
turma25 = cadastraTurma "Turma 25" 24 "aluno3" "2023.2"
 

turmas = [turma01, turma02, turma03, turma04, turma05, turma06, turma07,
        turma08, turma09, turma11, turma12, turma13, turma14, turma15,
        turma16, turma17, turma18, turma19, turma20, turma21, turma22,
        turma23, turma24, turma25]