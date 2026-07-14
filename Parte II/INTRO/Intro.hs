module Intro where

-- Questão 1
data DiaSemana = Segunda | Terça | Quarta | Quinta | Sexta | Sábado | Domingo deriving Eq

ehFimDeSemana :: DiaSemana -> Bool
ehFimDeSemana dia
                | dia == Sábado = True
                | dia == Domingo = True
                | otherwise = False

-- Questão 2
type Ponto2D = (Double,Double)

distanciaOrigem :: Ponto2D -> Double
distanciaOrigem (x,y) = sqrt ((x^2) + (y^2))


-- Questão 3
data Cliente = PessoaFisica String Int | PessoaJuridica String Int

obterNome :: Cliente -> String
obterNome (PessoaFisica nome idade) = nome
obterNome (PessoaJuridica razao idade) = razao

-- Questão 4
data ListaInt = Vazia | No Int ListaInt deriving Show

somaLista :: ListaInt -> Int
somaLista Vazia = 0
somaLista (No valor resto) = valor + somaLista resto


-- Questao 5

(>|) :: Int -> ListaInt -> ListaInt
(>|) x Vazia = No x Vazia
(>|) x (No valor resto) = No valor ((>|) x resto)

-- Questão 6

(|<) :: Int -> ListaInt -> ListaInt
(|<) x Vazia = No x Vazia
(|<) x (No valor resto) = No x (No valor resto)

-- Questão 7

data CorSemaforo = Vermelho | Amarelo | Verde deriving (Show,Eq)

proximaCor :: CorSemaforo -> CorSemaforo
proximaCor cor
             | cor == Verde = Amarelo
             | cor == Amarelo = Vermelho
             | otherwise = Verde

-- Questão 8
data Opcional a = Nenhum | Dado a 

filtrarValores :: [Opcional a] -> [a]
filtrarValores [] = []
filtrarValores (Nenhum : xs) = filtrarValores xs
filtrarValores (Dado x : xs) = x : filtrarValores xs