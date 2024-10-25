{-|
Module      : Turmas
Description : Módulo para gerenciamento de turmas e alunos.
-}

module Turmas (
    Turma,
    cadastraTurma,
    adicionarAlunos,
    getAlunos,
    ifVagasTurmas,
    getMediaTurma
) where

import Alunos

-- | Tipo de dado que representa uma turma.
-- A 'Turma' é composta por um nome, código, professor, período e uma lista de alunos.
data Turma = Turma {
    nome      :: String,    -- ^ Nome da turma.
    codigo    :: Int,       -- ^ Código identificador da turma.
    professor :: String,    -- ^ Nome do professor responsável.
    periodo   :: String,    -- ^ Período em que a turma está ativa.
    alunos    :: [Aluno]    -- ^ Lista de alunos matriculados na turma.
} deriving (Show, Eq)

-- | Número máximo de alunos permitido em uma turma.
maxAlunos :: Int
maxAlunos = 50

-- | Verifica se a turma está cheia.
-- Retorna 'True' se o número de alunos for igual ou maior que 'maxAlunos', caso contrário, 'False'.
ifTurmaCheia :: Turma -> Bool
ifTurmaCheia turma = length (alunos turma) >= maxAlunos

-- | Verifica a quantidade de vagas disponíveis na turma.
-- Retorna uma tupla onde o primeiro valor é o número de vagas restantes e o segundo valor é a capacidade máxima de alunos.
ifVagasTurmas :: Turma -> (Int, Int)
ifVagasTurmas turma = if ifTurmaCheia turma 
                       then (maxAlunos - length (alunos turma), maxAlunos) 
                       else (0, maxAlunos)

-- | Cria uma nova turma com os dados fornecidos.
-- Recebe o nome, código, professor e período da turma e retorna uma nova 'Turma' com uma lista de alunos vazia.
cadastraTurma :: String -> Int -> String -> String -> Turma
cadastraTurma nome codigo professor periodo = Turma { nome = nome, codigo = codigo, professor = professor, periodo = periodo, alunos = [] }

-- | Adiciona um aluno à turma.
-- Recebe um 'Aluno' e uma 'Turma' e retorna uma nova 'Turma' com o aluno adicionado, caso a turma não esteja cheia.
-- Caso a turma já esteja cheia, uma exceção é lançada.
adicionarAlunos :: Aluno -> Turma -> Turma
adicionarAlunos aluno turma
  | ifTurmaCheia turma = error "Turma cheia!"
  | otherwise = turma { alunos = aluno : alunos turma }

-- | Obtém a lista de alunos de uma turma.
-- Retorna a lista de alunos matriculados na 'Turma'.
getAlunos :: Turma -> [Aluno]
getAlunos turma = alunos turma

-- | Calcula a média das notas dos alunos na turma.
-- Retorna a média das notas dos alunos, sendo 0 se não houver alunos.
getMediaTurma :: Turma -> Float
getMediaTurma (Turma _ _ _ _ []) = 0
getMediaTurma (Turma _ _ _ _ alunos) = mediaAlunos alunos 0 0
  where
    mediaAlunos [] soma count = soma / realToFrac count -- Converte 'Int' para 'Float'
    mediaAlunos (aluno:alunos) soma count = mediaAlunos alunos (soma + getMedia aluno) (count + 1)
