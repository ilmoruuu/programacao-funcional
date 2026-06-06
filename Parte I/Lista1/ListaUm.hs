module ListaUm where

-- Questão 1 
igual ::  Eq a => a -> a -> a -> Int
igual a b c
        | (a == b) && (c == a) = 3
        | (a == b) || (a == c) || (b == c) = 2
        | otherwise = 0

-- Questão 2

media :: Float -> Float -> Float -> Float
media a b c = (a + b + c) / 3.0

maiorMedia :: Float -> Float -> Float -> Int
maiorMedia a b c
            | a > med && b > med && c > med = 3
            | (a > med || b > med) && (b> med || c > med) = 2
            | a > med || b > med || c > med = 1
            | otherwise = 0
        where med = media a b c


-- Questão 3
potencia_2 :: Num a => a -> a
potencia_2 a = a * a


-- Questão 4
potencia_4 :: Num a => a -> a
potencia_4 a = potencia_2 (potencia_2 a)

-- Questão 5
xor :: Bool -> Bool -> Bool
xor a b
        | a /= b = True
        | otherwise = False


-- Questão 6
x_maior :: Float -> Float -> Float -> Float
x_maior a b c = (-b + sqrt (b^2 - 4 * a * c)) / 2 * a

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = (-b - sqrt (b^2 - 4 * a * c)) / 2 * a


-- Questão 7
-- Com Limite
somaCL :: Int -> Int -> Int
somaCL a b = sum [a..b]

--Sem Limite
somaSL :: Int -> Int -> Int
somaSL a b = sum (tail lista)
        where lista = init [a..b]


-- Questão 8 
multiplos :: Int -> Int -> Int -> [Int]
multiplos a b c = filter (\x -> x `mod` c == 0) [a..b]


-- Questão 9
multi :: Int -> Int -> Int
multi a b = sum (map (*a) [b])


-- Questão 10
mod2 :: Float -> Float -> Float
mod2 _ 0 = error "ERRO! Divisão por 0"
mod2 a b
        | b > a = a
        | otherwise = mod2 (a - b) b


-- Questão 11
aproxima3 :: Int -> Float
aproxima3 1 = sqrt 6
aproxima3 n = sqrt (6 + aproxima3 (n-1))


-- Questão 12
maneiras :: Int -> [a] -> Int
maneiras 0 _ = error "Não existe elementos"
maneiras a xs = (product [1..numE]) `div` ((product [1..a]) * (product [1..dif]))
        where numE = length xs
              dif = numE - a

-- Questão 13
maiorPosicao :: [Int] -> (Int, Int)
maiorPosicao [] = error "Lista Vazia"
maiorPosicao xs = (maior, posicao)
                where maior = maximum xs
                      posicao = length (takeWhile (< maior) xs)

-- Questão 14
converter :: [Int] -> [String]
converter [] = []
converter xs = map (snd) [lista !! head xs] ++ converter (tail xs)
                where lista = [(0,"zero"),(1,"um"),(2,"dois"),(3,"tres"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"), (9,"nove")]

-- Questão 15
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n [] _ = error "Lista vazia!"
del_posicao_n xs y = take y xs ++ drop (y+1) xs


-- Questão 16
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x xs pos num = take pos xs ++ [num] ++ drop (pos) xs


-- Questão 17
valorN :: Int -> [a] -> a
valorN _ [] = error "Não existe elementos"
valorN a xs = xs !! a


-- Questão 18
mergear :: Ord a => [a] -> [a] -> [a]
mergear xs [] = xs
mergear [] ys  = ys
mergear xs ys
                | head xs < head ys = head xs : mergear (tail xs) ys
                | otherwise = head ys : mergear xs (tail ys)


-- Questão 19
intersecao :: Ord a => [a] -> [a] -> [a]
intersecao xs [] = []
intersecao [] ys = []
intersecao xs ys
                | elem (head xs) ys == True = head xs : intersecao (tail xs) ys
                | otherwise = intersecao (tail xs) ys


-- Questão 20
agrupar :: String -> [[Char]]
agrupar [] = []
agrupar (x:xs) = grupo : agrupar resto
  where
    (iguais, resto) = span (== x) xs
    grupo = x : iguais
