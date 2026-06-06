module Note where

-- Questão 1
filtraPrecos :: Float -> [(String, Float)] -> [String]
filtraPrecos _ [] = []
filtraPrecos n xs = [fst y | y <- xs, snd y > n]

-- Questão 2
primos :: Int -> Bool
primos 0 = False
primos x 
        | (length [y | y <- [1..x], x`mod`y == 0]) == 2 = True
        | otherwise = False

soPrimos :: [Int] -> [Int]
soPrimos [] = []
soPrimos xs = [y | y <- xs, (primos y) == True]

-- Questão 3
intercala :: [a] -> [a] -> [a]
intercala xs [] = xs
intercala [] ys = ys
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- Questão 4
converter :: Int -> String
converter 0 = "00:000"
converter m 
        | m < 0 = error "Distância negativa!"
        | km < 10 = "0" ++ show km ++ ":" ++ show mts 
        | otherwise = show km ++ ":" ++ show mts 
        where
        km = m `div` 1000
        mts = m `mod` 1000

-- Questão 5
projetos :: Int -> [(String, [Int])] -> [String]
projetos _ [] = []
projetos hrs xs = [fst z | z <- xs, (sum (snd z)) > hrs]

-- Questão 6
removerOcorrencias :: Eq a => a -> Int -> [a] -> [a]
removerOcorrencias _ _ [] = []
removerOcorrencias letra vzs (x:xs)
                                | vzs == 0 = (x:xs)
                                | x == letra = removerOcorrencias letra (vzs-1) xs
                                | otherwise = x : removerOcorrencias letra vzs xs

-- Questão 7
formataMoeda :: Int -> String
formataMoeda x
        | x < 10 = "R$ 0,0" ++ show x
        | x < 100 = "R$ 0," ++ show x
        | x > 100 && cents < 10 = "R$" ++ show reais ++ ",0" ++ show cents
        | otherwise =  "R$" ++ show reais ++ "," ++ show cents
        where
                reais = x `div` 100
                cents = x `mod` 100

-- Questão 8
meuAny :: (a-> Bool) -> [a] -> Bool
meuAny _ [] = False
meuAny fun xs = if (length verifica) > 0 then True else False
        where
                verifica = [y | y <- xs, fun y]