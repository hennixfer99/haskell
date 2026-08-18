
{-
public Integer somarUm(Integer x){
return x + 1;
}
-}
somarUm :: Int -> Int 
somarUm x = x + 1

saudacao :: String -> [Char]
saudacao nome = "Seja bem vind " ++ nome

listaNumeros :: [Int]
listaNumeros = [1,2,3,4,5]

ehMaior :: Int -> Bool
ehMaior x =  x >= 18

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x ^ 2


-- Pattern matching 


z :: (Int, Int) -> Int
z     (1,0) = 1
z     (0,1) = 0
z     (x,30) = x + 80
z     (30,x) = x
z      tupla = fst tupla

diasSemana :: Int -> String
diasSemana 0 = "Domingo"
diasSemana 1 = "Segunda"
diasSemana x = "Qualquer outro dia: " ++ show x

-- _ = phodace

-- tipo de dados algebricos. São os tipos que mapeiam a regra de negocio com o programa.Applicative


-- Data Constructor  =   Values Constructors
data Booleano = Verdade | Falso 

intParaBool :: Int -> Bool
intPAraBool 0 = False
intParaBool 1 = True