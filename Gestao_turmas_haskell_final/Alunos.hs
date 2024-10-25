{-|
Module      : Alunos
Description : Módulo para gerenciamento de informações sobre pessoas e alunos.
-}

module Alunos(
  Pessoa,
  Aluno,
  criaPessoa,
  cadastraAluno,
  getNome,
  getNomeAluno,
  getCodigoAluno,
  getNotas,
  getMedia,
  ifAprovado,
  pessoateste,
  alunoteste
) where

-- | Tipo de dado que representa uma pessoa.
-- A 'Pessoa' é composta por nome, CPF e data de nascimento.
data Pessoa = Pessoa {
    nome           :: String, -- ^ Nome da pessoa.
    cpf            :: String, -- ^ CPF da pessoa.
    dataNascimento :: String  -- ^ Data de nascimento da pessoa.
} deriving (Show, Eq)

-- | Tipo de dado que representa um aluno.
-- O 'Aluno' é composto por uma 'Pessoa', matrícula, status, data de início, contato e uma lista de notas.
data Aluno = Aluno {
    cadastro    :: Pessoa,     -- ^ Dados pessoais do aluno.
    matricula   :: Int,        -- ^ Número de matrícula do aluno.
    status      :: Bool,       -- ^ Status de aprovação do aluno.
    dataInicio  :: String,     -- ^ Data de início das atividades do aluno.
    contato     :: String,     -- ^ Contato do aluno.
    totalNotas  :: [Float]     -- ^ Lista de notas do aluno.
} deriving (Show, Eq)

-- | Cria uma nova pessoa com os dados fornecidos.
-- Recebe o nome, CPF e data de nascimento e retorna uma nova 'Pessoa'.
criaPessoa :: String -> String -> String -> Pessoa
criaPessoa nome cpf dataNascimento = Pessoa { nome = nome, cpf = cpf, dataNascimento = dataNascimento }

-- | Cria um novo aluno com os dados fornecidos.
-- Recebe uma 'Pessoa', matrícula, status, data de início, contato e uma lista de notas e retorna um novo 'Aluno'.
cadastraAluno :: Pessoa -> Int -> Bool -> String -> String -> [Float] -> Aluno
cadastraAluno (Pessoa nome cpf dataNascimento) matricula status dataInicio contato totalNotas = 
    Aluno { cadastro = Pessoa nome cpf dataNascimento, matricula = matricula, status = status, dataInicio = dataInicio, contato = contato, totalNotas = totalNotas }

-- | Obtém o nome de uma pessoa.
-- Recebe uma 'Pessoa' e retorna o nome dela.
getNome :: Pessoa -> String
getNome (Pessoa nome _ _) = nome

-- | Obtém o nome de um aluno.
-- Recebe um 'Aluno' e retorna o nome do aluno acessando o campo 'cadastro'.
getNomeAluno :: Aluno -> String
getNomeAluno aluno = nome (cadastro aluno) 

-- | Obtém o código de matrícula de um aluno.
-- Recebe um 'Aluno' e retorna o número da matrícula.
getCodigoAluno :: Aluno -> Int
getCodigoAluno (Aluno _ matricula _ _ _ _) = matricula

-- | Obtém as notas de um aluno.
-- Recebe um 'Aluno' e retorna a lista de notas (considera-se que o aluno possui exatamente 3 notas).
getNotas :: Aluno -> [Float]
getNotas (Aluno _ _ _ _ _ notas) = notas

-- | Calcula a média das notas de um aluno.
-- Recebe um 'Aluno' e retorna a média das notas (considera-se que o aluno possui exatamente 3 notas).
getMedia :: Aluno -> Float
getMedia (Aluno _ _ _ _ _ [n1,n2,n3]) = (n1 + n2 + n3) / 3 

-- | Verifica a situação do aluno com base na média das notas.
-- Retorna "Aprovado" se a média for 6.0 ou mais, "Recuperacao" se a média for entre 5.0 e 5.9, e "Reprovado" caso contrário.
ifAprovado :: Aluno -> String
ifAprovado aluno = case () of
  _ | getMedia aluno >= 6.0 -> "Aprovado"
    | getMedia aluno >= 5.0 -> "Recuperacao"
    | otherwise -> "Reprovado"

-- | Objeto de teste para a criação de uma 'Pessoa'.
pessoateste :: Pessoa
pessoateste = criaPessoa "Joao" "123456789" "1990-01-01"

-- | Objeto de teste para a criação de um 'Aluno'.
alunoteste :: Aluno
alunoteste = cadastraAluno pessoateste 123 True "2022-01-01" "joao@example.com" [5.1,5.4,90.0]
