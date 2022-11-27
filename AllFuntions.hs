cinco :: Int -> Int
cinco x = 5


cuadrado :: Int -> Int
cuadrado x = x^2


identidad :: Int -> Int
identidad x = x

mitad :: Int -> Float
mitad :: Fractional a => a -> a
mitad x = x / 2


esPositivo :: Int -> Bool
esPositivo x = if 0 < x then True else False 


areaR :: Int -> Int -> Int
areaR x y = x * y


-- areaT :: Int -> Int -> Float
areaT :: Fractional a => a -> a -> a
areaT x y = x * y / 2


par ::Int -> Bool
par x = if ((x `mod` 2) == 0) then True else False


entre0y8 :: Int -> Bool
entre0y8 x = if ((x > 0) && (x < 8)) then True else False


media3 x y z = (x + y + z) / 3


potencia :: Int -> Int -> Int
potencia x y = y^x


xor1 :: Bool -> Bool -> Bool
xor1 a b = ((not a) && b) || (a && (not b))


fac 0 = 1
fac x = x * fac(x - 1)

monedas (x:xs) = xs ++ xs
-- 0 0 0 0 1
-- 0 0 8 0 3
-- 1 1 1 1 1
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = a + b*2 + c*5 + d*10 + e*20


doble :: Num a => a -> a
doble x = x * 2


vacia [] = 1
vacia (x:xs) = x * vacia(xs)


--b Int -> Bool
numeroPar :: Int -> Bool
numeroPar x = if (x `mod` 2 == 0) then True else False 


greater :: Int -> Int -> Bool 
greater x y = if x > y then True else False


entreCeroYnueve :: Int -> Bool
entreCeroYnueve x = if 0 < x && x < 9 then True else False


multiploDeTres :: Int -> Bool
multiploDeTres x = if (x `mod` 3 == 0) then True else False


mayorTresNumeros :: Int -> Int -> Int -> Int
mayorTresNumeros x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | z >= x && z >= y = z


xor :: Bool -> Bool -> Bool
xor a b
    | a == True && b == True = False
    | a == False && b == False = False
    | (a == True && b == False) || (a == False && b == True) = True


media3 :: Double -> Double -> Double -> Double
media3 x y z = (x + y + z) / 3 


lista1 = [2, 4, 6, 8]
sumaDeCuadrados :: Num a => [a] -> [a]
sumaDeCuadrados lista = [numero ^ 2 | numero <- lista]


mostrarVocales frase = [letra | letra <- frase, letra `elem` ['a','e','i','o','u']]


longitud lista = sum[1 | x<- lista]

-- Obtener los dobles de los quince primeros números naturales
numNaturales lista = [x *2 | x<- lista]

-- mostrar los valores iguales o mayores a 16
numA16 lista = [x * 2 |x <- lista, x*2 >=16]


-- Obtener todos los números del 50 al 100 cuyo resto al dividir por 7 fuera 3
de50a100 lista = [ x | x<- lista, x`mod` 7 == 3]


altoBajo xs= [if x < 10 then "Alto!" else "Bajo!" | x <-xs, odd x]


--let lista =[x + y | x<- [1..15], y<-[1..50], x<10, y `mod` 2 ==0]


productoLista [] = 1
productoLista (x:xs) = x * productoLista xs   


todosPares lista = if ([x  | x <- lista, odd x ] == lista) then "Impar" else "Par"


mostrarVocales frase = [ letra | letra <- frase, letra `elem` ['a','e','i','o','u']]


--(lista doble:
--   let lista = [x + y | x <- [1..50], y <- [1..100], x <10, y `mod` 10 == 0]


todosImpares xs = ([x |x <- xs , odd x]) == xs 


sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [(xc+x) | (xc,x)<- zip xs (tail xs)]


repiteC :: Int -> [a] -> [a]
repiteC n xs = concat[replicate n x | x <- xs]


insertaenposicion :: a -> Int -> [a] -> [a]
insertaenposicion x n xs = take n xs ++ (x:drop n xs)


numeroMayor a b = 10 * max a b + min a b

finales n xs = reverse(take n (reverse xs))


sumaCuadrados :: Int -> Int
sumaCuadrados n = sum(map cuadrados [read [x] | x <- show n])
    where cuadrados x = x ^ 2


incidenciasR x ys = sum[1 | s <- ys, x == s]


factorial :: Int -> Int
factorial n = if n == 0 then 1
            else n * factorial(n - 1)


sumaCifras :: Integer -> Integer
sumaCifras xs = sum([read [x] | x <- show xs])


diagonal ::[[a]] -> [a]
diagonal xss = [xs!!x | (xs,x) <- zip xss [0..n]]
    where n = length (head xss) - 1


rango lista = [maximum lista] ++ [minimum lista]


rango1 lista = [minimum lista, maximum lista]


palindromo lista = lista == reverse lista


interior lista =  init(tail lista)


finales1 n lista = drop n lista


finales n lista = drop (length lista - n) lista


segmento m n lista = drop (m-1) (take n lista)


extremos n lista = take n lista ++ drop (length lista - n) lista


calculaPi n = 4 * sum [(-1)**x/(2*x+1) | x <- [0..n]]


diagonal :: [[a]] -> [a]
diagonal xss = [xs!!k | (xs,k) <- zip xss [0..n]]
    where n = length (head xss) - 1


listasMayoresWhere :: [[Int]] -> [[Int]]
listasMayoresWhere xss = [xs | xs <- xss, sum xs == m]
    where m = maximum [sum xs | xs <- xss]


extremos n xs = take n xs ++ reverse(take n (reverse xs))


extremosDrop n xs = take n xs ++ drop (length xs - n) xs


todosParesAll xs = all(==True)[even x |x <- xs]

sumaCuadrados2 :: Int -> Int
sumaCuadrados2 n = sum(map cuadrados [read [x] | x <- show n])
where cuadrados x = x ^2


listasMayores :: [[Int]] -> [[Int]]
listasMayores xss = [xs | xs <- xss, sum xs == m]
    where m = maximum [sum xs | xs <- xss]



absolute x
        | x < 0     = -x
        | otherwise =  x