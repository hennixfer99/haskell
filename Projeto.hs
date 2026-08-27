module Projeto where

data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show

data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Pessoa Estagiario _) = 1500
verSalario (Pessoa Programador _) = 5750.15
verSalario (Pessoa Coordenador _) = 8000
verSalario (Pessoa Gerente _) = 10807.20

verFolha :: Pessoa -> String
verFolha p = "{nome: \"" ++ (nome p) ++ "\", cargo: \"" ++ show (cargo p) ++ "\", salario: " ++ show (verSalario p) ++ "}"

promover :: Pessoa -> Pessoa
promover (Pessoa Estagiario n) = Pessoa Programador n
promover (Pessoa Programador n) = Pessoa Coordenador n
promover (Pessoa _ n) = Pessoa Gerente n

-- ex31
data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

perguntas :: [Pergunta] -> [Int]
perguntas x = map pergNum x

and' :: Pergunta -> Pergunta -> Int
and' Sim Sim = pergNum Sim
and' Nao Nao = pergNum Nao
and' _ _ = pergNum Nao

or' :: Pergunta -> Pergunta -> Int
or' Sim _ = pergNum Sim
or' _ Sim = pergNum Sim
or' _ _ = pergNum Nao

not' :: Pergunta -> Pergunta -> Int
not' Sim Sim = pergNum Nao
not' Nao Nao = pergNum Sim
not' _ _ = pergNum Sim


-- ex32
data Graus = Celsius | Fahrenheit | Kelvin deriving Show

converterCelsius :: Double -> Graus -> Double
converterCelsius x Fahrenheit = (x - 32) * 5/9 
converterCelsius x Kelvin = x + 273.1
converterCelsius x Celsius = x
-- converterCelsius _ _ = "Não pode inserir String, caso tenha inserido valor deve escolher entre Kelvin ou Farenheit" 

converterKelvin :: Double -> Graus -> Double
converterKelvin x Fahrenheit = ((x - 32) * 5/9) + 273.15 
converterKelvin x Celsius = x + 273.1
converterKelvin x Kelvin = x
-- converterKelvin _ _ = "Não pode inserir String, caso tenha inserido valor deve escolher entre Celsius ou Farenheit"

converterFahrenheit :: Double -> Graus -> Double
converterFahrenheit x Celsius = (x * 9/5) + 32 
converterFahrenheit x Kelvin = (x - 273.1) * 9/5 + 32
converterFahrenheit x Fahrenheit = x
-- converterFahrenheit _ _ = "Não pode inserir String, caso tenha inserido valor deve escolher entre Kelvin ou Celsius"

-- ex33
data Jogadas = Pedra | Papel | Tesoura deriving Show

jogada :: Jogadas -> Jogadas -> String

jogada Pedra Papel = "Papel ganha"
jogada Pedra Tesoura = "Pedra ganha"
jogada Pedra Pedra = "Empate! de Pedra"

jogada Papel Pedra = "Papel ganha"
jogada Papel Tesoura = "Tesoura ganha"
jogada Papel Papel = "Empate! de Papel"

jogada Tesoura Pedra = "Pedra ganha"
jogada Tesoura Papel = "Tesoura ganha"
jogada Tesoura Tesoura = "Empate! de Tesoura"

jogada _ _ = "Insira apenas Pedra, Papel ou Tesoura como 2 pârametros"

-- ex34

ex34 xs = [x | x <- xs, x `elem` "AEIOUaeiou"]

-- ex35 

data Imperial = Lin | Yd | Ft deriving Show

converterImperial :: Imperial -> Imperial -> Double -> Double 

converterImperial Lin Yd x = x * 36
converterImperial Lin Ft x = x *12
converterImperial Ft Lin x = x / 12
converterImperial Ft Yd x = x * 3
converterImperial Yd Lin x = x / 36
converterImperial Yd Ft x = x / 3
converterImperial _ _ x = x 

converterMetros :: Double -> Imperial -> Double

converterMetros x Yd = x * 1.094
converterMetros x Ft = x * 3.28084
converterMetros x Lin = x * 39.3701
converterMetros x _ = x

-- ex36

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving Show

checaFim :: Mes -> Int

checaFim Janeiro = 31
checaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

prox :: Mes -> Mes

prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

data Hemisferio = Norte | Sul deriving Show
data Estacao = Verao | Outono | Inverno | Primavera deriving Show

estacao :: Mes -> Hemisferio -> Estacao

estacao Janeiro Sul= Verao
estacao Fevereiro Sul= Verao
estacao Marco Sul= Outono
estacao Abril Sul= Outono
estacao Maio Sul= Outono
estacao Junho Sul= Inverno
estacao Julho Sul= Inverno
estacao Agosto Sul= Inverno
estacao Setembro Sul= Primavera
estacao Outubro Sul= Primavera
estacao Novembro Sul= Primavera
estacao Dezembro Sul= Verao

estacao Janeiro Norte= Inverno
estacao Fevereiro Norte= Inverno
estacao Marco Norte= Primavera
estacao Abril Norte= Primavera
estacao Maio Norte= Primavera
estacao Junho Norte= Verao
estacao Julho Norte= Verao
estacao Agosto Norte= Verao
estacao Setembro Norte= Outono
estacao Outubro Norte= Outono
estacao Novembro Norte= Outono
estacao Dezembro Norte= Inverno
--ex 37

checkPalindromo :: String -> Bool

checkPalindromo x = reverse x == x 
checkPalindromo _ = False

--ex38 

elMultiplosde7 xs = reverse (filter (\x -> x `mod` 7 == 0 && even x && x < 0 && odd x) xs)

checkMultiplosde7 :: [Int] -> [Int]
checkMultiplosde7 x = elMultiplosde7 x
checkMultiplosde7 _ = [0]

--ex39

coords :: String -> String -> String -> (String, String, String)

coords x y z = (reverse x, reverse y, reverse z);

--ex3.10

revNum :: String -> Int -> String

revNum s n =  reverse (take n s) ++ reverse (take (length s - n) (reverse s))

--ex3.11

data Binario = Zero | Um deriving Show
data Funcao = Soma2 | Maior | Menor | Mult2 deriving Show

aplicar :: Funcao -> Binario -> Binario -> Binario

aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Zero = Um
aplicar Soma2 Um Um = Zero

aplicar Maior Zero Zero = Zero
aplicar Maior Zero Um = Um
aplicar Maior Um Zero = Um
aplicar Maior Um Um = Um

aplicar Menor Zero Zero = Zero
aplicar Menor Zero Um = Zero
aplicar Menor Um Zero = Zero
aplicar Menor Um Um = Um

aplicar Mult2 Zero Zero = Zero
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Um Zero = Zero
aplicar Mult2 Um Um = Um

--ex3.12

-- binList :: [Binario] -> [Int]

myst :: (Num a, Ord a) => [a] -> [a]
myst [] = []
myst (x:xs) = myst smaller ++ [x] ++ myst bigger
    where
        smaller = filter (< x) xs
        bigger = filter (>= x) xs

