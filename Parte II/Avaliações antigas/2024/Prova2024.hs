module Prova2025 where

-- Questão 01

data ListaAninhada a = Elem a | Lista [ListaAninhada a]

planificar :: ListaAninhada a -> [a]
planificar (Lista []) = []
planificar (Elem a) = [a]
planificar (Lista (x:xs)) = planificar x ++ planificar (Lista xs)


-- Questão 02

data Expr = Lit Int | Soma Expr Expr | Sub Expr Expr | Mult Expr Expr | Div Expr Expr

avalia :: Expr -> Int
avalia (Lit x) = x
avalia (Soma x y) = avalia x + avalia y
avalia (Sub x y) = avalia x - avalia y
avalia (Mult x y) = avalia x * avalia y
avalia (Div x y) = avalia x `div` avalia y


-- Questão 03

data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving Show

repeat' :: a -> Arvore a
repeat' x = No (repeat' x) x (repeat' x)

take' :: Int -> Arvore a -> Arvore a
take' _ Folha = Folha
take' 0 _ = Folha
take' n (No esq a dir) = No (take' (n-1) esq) a (take' (n-1) dir)

replicate' :: Int -> a -> Arvore a
replicate' n x = take' n (repeat' x)


-- Questão 4
raizes :: (Float, Float, Float) -> [Maybe Float]
raizes (a, b, c)
    | delta < 0 = [Nothing]
    | otherwise = [Just x1, Just x2]
    where
        delta = b^2 - 4*a*c
        x1 = (-b + sqrt delta) / (2 * a)
        x2 = (-b - sqrt delta) / (2 * a)