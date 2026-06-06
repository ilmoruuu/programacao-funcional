module ListaDois where

-- Questão 1
quadrados :: Int -> Int
quadrados 0 = error "Número menor que 1"
quadrados n = sum [x * x | x <- [1..n] ]

-- Questão 2
grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b)| a <- [0..x], b <- [0..y]]


-- Questão 3
quadrado :: Int -> [(Int,Int)]
quadrado n  = [(x,y) | x <- [0..n], y <- [0..n], x /= y ]

-- Questão 4
replicate' :: Int -> a -> [a]
replicate' v o  = [o | _ <- [1..v]]

-- Questão 5
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n], ((x*x) + (y*y)) == (z*z)]

-- Questão 6
divisoresMenos :: Int -> [Int]
divisoresMenos 0 = []
divisoresMenos x = [y | y <- [1..x-1], x `mod` y  == 0]

perfeitos :: Int -> [Int]
perfeitos 0 = []
perfeitos x = [ y | y <- [1..x], y == sum(divisoresMenos y)]

-- Questão 7
junta _ _ = [(x,y) | x <- [1..2], y <- [3..4]]