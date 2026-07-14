module ListaQuatro where
import Data.List (sort)

-- Questão 1
impares :: [Int] -> [Int]
impares xs = sort (filter odd xs)


-- Questão 2
posicao :: Int -> [a] -> a
posicao _ [] = error "Lista vazia"
posicao 0 xs = head xs
posicao p l
            | p > length l = error "Fora de escopo"
            | otherwise = posicao (p-1) (tail l)

-- Questão 3
repete :: Int -> [[Int]]
repete 0 = []
repete n
        | n < 0 = error "Valor negativo!"
        | otherwise = [take y (repeat y) | y <- [n,n-1..1]]

-- Questão 4
palindromo :: Eq a => [a] -> Bool
palindromo xs = and (avalia (teste xs))

teste :: [a] -> [(a,a)]
teste xs = zip xs (reverse xs)

avalia :: Eq a => [(a,a)] -> [Bool]
avalia xs = [a == b | (a,b) <- xs]

-- Questão 5
fibonacci :: Int -> [Int]
fibonacci f = take f loop

loop :: [Int]
loop = 0 : 1 : zipWith (+) loop (tail loop)


-- Questão 6

-- a)
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = False
all' fun xs = and (map fun xs)


-- b)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' fun xs = or (map fun xs)

-- c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' cond (x:xs)

    | cond x    = x : takeWhile' cond xs
    | otherwise = []

-- d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' cond l@(x:xs)
        | cond x    = dropWhile' cond xs
        | otherwise = l


-- Questão 7
map' :: (a -> b) -> [a] -> [b]
map' fun xs = foldr (\x y -> fun x : y) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' cond xs = foldr (\x y -> if cond x then x : y else y) [] xs

-- Questão 8
dec2int :: [Int] -> Int
dec2int l = read trans :: Int
        where
            trans = foldr (\x y -> show x ++ y) "" l

-- Questão 9
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
            | p x = []
            | otherwise = h x : unfold p h t (t x)

mapDois :: (a->b) -> [a] -> [b]
mapDois f = unfold null (\xs -> f (head xs)) tail

-- Questão 10
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

-- Questão 11
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

unCurry' :: (a -> b -> c) -> ((a, b) -> c)
unCurry' f = \(x,y) -> f x y
