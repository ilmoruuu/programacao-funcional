module ExercicioUm where
main :: IO ()

-- Questão 3 --
meuLast :: [a] -> a
meuLast [] = error "Lista vazia"
meuLast xs = head (reverse xs)


-- Questão 4 --
meuInit :: [a] -> [a]
meuInit [] = []
meuInit xs = reverse (drop 1 (reverse xs) )
main = print (meuInit[1,2,3])