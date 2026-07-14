module ListaCinco where

-- Questão 1

data Nat = Zero | Suc Nat deriving Show

somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc (somar m n)

mult :: Nat -> Nat -> Nat 
mult _ Zero = Zero
mult Zero _ = Zero
mult (Suc a) b = somar b (mult a b)


-- Questão 2

data Arvore a = Folha a | No (Arvore a) a (Arvore a)

-- existe :: Eq a => a -> Arvore a -> Bool
-- existe x (Folha y) = x == y
-- existe x (No esq y dir) = x == y || existe x esq || existe x dir

existe' :: Ord a => a -> Arvore a -> Bool
existe' x (Folha y) = x == y 
existe' x (No esq y dir)
                        | compare x y == LT = existe' x esq
                        | compare x y == GT = existe' x dir
                        | otherwise = True

-- Questão 3

data Arvore' a = Folha' a | No' (Arvore' a) (Arvore' a) deriving Show

qntd :: Arvore' a -> Int
qntd (Folha' a) = 1
qntd (No' esq dir) = qntd dir + qntd esq

balanceada :: Arvore' a -> Bool
balanceada (Folha' _) = True
balanceada (No' esq dir) = abs (qntd esq - qntd dir) <= 1 && balanceada esq && balanceada dir

-- Questão 4

dividir :: [a] -> ([a],[a])
dividir [] = ([],[])
dividir l = (esq, dir)
        where
            esq = take met l
            dir = drop met l
            met = div (length l) 2

balancear :: [a] -> Arvore' a
balancear [a] = Folha' a
balancear l = No' (balancear (fst (dividir l))) (balancear (snd (dividir l)))

-- Questão 5

data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val x) = x
avaliar (Add x y) =  avaliar x + avaliar y