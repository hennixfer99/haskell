module Projeto where

-- data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show

-- data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

-- verSalario :: Pessoa -> Double
-- verSalario (Pessoa Estagiario _) = 1500
-- verSalario (Pessoa Programador _) = 5750.15
-- verSalario (Pessoa Coordenador _) = 8000
-- verSalario (Pessoa Gerente _) = 10807.20

-- verFolha :: Pessoa -> String
-- verFolha p = "{nome: \"" ++ (nome p) ++ "\", cargo: \"" ++ show (cargo p) ++ "\", salario: " ++ show (verSalario p) ++ "}"

-- promover :: Pessoa -> Pessoa
-- promover (Pessoa Estagiario n) = Pessoa Programador n
-- promover (Pessoa Programador n) = Pessoa Coordenador n
-- promover (Pessoa _ n) = Pessoa Gerente n

-- module ExDoLivro where

data Pergunta = Sim | Nao deriving Show

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

-- pergNum _ = error "pergNum: argumento invalido"

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

ex34 xs = [x | x <- xs, x `elem` "AEIOUaeiou"]
