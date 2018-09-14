module Main exposing (..)


esPrimo : Int -> Bool
esPrimo n = esPrimoN 2 n
esPrimoN cont n =
    if n == 2 then True
        else
    if modBy cont n == 0
        then False
    else if cont == n - 1
            then True
        else esPrimoN (cont + 1) n

fibonacci : Int -> Int
fibonacci n = case n of
  0 -> 0
  1 -> 1
  2 -> 1
  _ -> fibonacci (n-1) + fibonacci (n-2)

primos : Int -> List Int 
primos x = 
    if x < 2
        then []
    else if esPrimo x == False
        then primos (x - 1) 
    else
        x :: primos (x - 1)

nPrimos: Int -> List Int
nPrimos n = contn (n,2) 
contn (n,y) =  
    if n == 0 
    then [] 
    else if esPrimo y == False
    then contn (n, y + 1)
    else y:: contn (n - 1, y + 1)
 