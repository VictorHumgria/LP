-- GestaoTurmas.hs
module Main where

import Turmas
import Alunos
import BancoDados

-- Função principal que administra o sistema
main :: IO ()
main = do
  putStrLn "Sistema de Gestão de Turmas"
  putStrLn "-------------------------------"
  putStrLn "1. Cadastrar Turma"
  putStrLn "2. Cadastrar Aluno"
  putStrLn "3. Adicionar Aluno à Turma"
  putStrLn "4. Verificar Vagas em Turma"
  putStrLn "5. Calcular Média da Turma"
  putStrLn "6. Sair"
  escolha <- readLn :: IO Int
  case escolha of
    1 -> cadastrarTurma
    2 -> cadastrarAluno
    3 -> adicionarAlunoTurma
    4 -> verificarVagasTurma
    5 -> calcularMediaTurma
    6 -> putStrLn "Saindo do sistema..."
    _ -> putStrLn "Opção inválida. Tente novamente."

-- Função para cadastrar uma turma
cadastrarTurma :: IO ()
cadastrarTurma = do
  putStrLn "Cadastrar Turma"
  putStrLn "---------------"
  nome <- getLine
  codigo <- readLn :: IO Int
  professor <- getLine
  periodo <- getLine
  let turmaMain = cadastraTurma nome codigo professor periodo
  putStrLn "Turma cadastrada com sucesso!"
  main

-- Função para cadastrar um aluno
cadastrarAluno :: IO ()
cadastrarAluno = do
  putStrLn "Cadastrar Aluno"
  putStrLn "--------------"
  putStrLn "Nome:"
  nome <- getLine
  putStrLn "Cpf:"
  cpf <- getLine
  putStrLn "Data Nascimento:"
  dataNascimento <- getLine
  putStrLn "Matricula:"
  matricula <- readLn
  putStrLn "Data inicio:"
  dataInicio <-getLine
  putStrLn "Contato:"
  contato <- getLine
  let alunoMain = cadastraAluno (criaPessoa nome cpf dataNascimento) matricula True dataInicio contato
  putStrLn "Aluno cadastrado com sucesso!"
  main

-- Função para adicionar um aluno à uma turma
adicionarAlunoTurma :: IO ()
adicionarAlunoTurma = do
  putStrLn "Adicionar Aluno à Turma"
  putStrLn "------------------------"
  codigoTurma <- readLn :: IO Int
  let turma = getTurma codigoTurma
  nomeAluno <- getLine
  cpfAluno <- getLine
  dataNascimentoAluno <- getLine
  let pessoa = criaPessoa nomeAluno cpfAluno dataNascimentoAluno
  let aluno = cadastraAluno pessoa 0 True "" "" []
  let turmaAtualizada = adicionarAlunos aluno turma
  putStrLn "Aluno adicionado à turma com sucesso!"
  main

-- Função para verificar vagas em uma turma
verificarVagasTurma :: IO ()
verificarVagasTurma = do
  putStrLn "Verificar Vagas em Turma"
  putStrLn "-------------------------"
  codigoTurma <- readLn :: IO Int
  let turma = getTurma codigoTurma
  let vagas = ifVagasTurmas turma
  putStrLn $ "Vagas disponíveis: " ++ show vagas
  main

-- Função para calcular a média de uma turma
calcularMediaTurma :: IO ()
calcularMediaTurma = do
  putStrLn "Calcular Média da Turma"
  putStrLn "------------------------"
  codigoTurma <- readLn :: IO Int
  let turma = getTurma codigoTurma
  let media = getMediaTurma turma
  putStrLn $ "Média da turma: " ++ show media
  main

getTurma :: Int -> Turma
getTurma codigo = turmas !! (codigo - 1)