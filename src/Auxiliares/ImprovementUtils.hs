module Auxiliares.ImprovementUtils where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ShowOrder as ShowOrder

import qualified Construtivas.Aleatoria as Aleatoria
import qualified Construtivas.Gulosa as Gulosa
import qualified Construtivas.ParcialmenteGulosa as ParcialmenteGulosa

initialization :: IO [Int]
initialization = do
    putStrLn "Selecione uma solução inicial: "
    putStrLn "1 - Aleatoria"
    putStrLn "2 - Gulosa / Vizinho mais próximo"
    putStrLn "3 - Parcialmente Gulosa"
    option <- readLn
    case option of
        1 -> do
            list <- Aleatoria.permute [0..50]
            initial list
        2 -> do
            list <- Gulosa.selecaoGulosa [0] [1..50]
            initial list
        3 -> do
            list <- ParcialmenteGulosa.selecaoParcial [0] [1..50]
            initial list
        _ -> putStrLn "Error" >> initialization
  where
    initial :: [Int] -> IO [Int]
    initial list = do
        ShowOrder.main list
        total <- Distancias.somaDistancias list
        putStrLn (show total)
        return list

extractNeighbors :: Int -> Int -> [a] -> ((a,a,a), (a,a,a))
extractNeighbors i j xs
    | i == j    = error "i and j must be different"
    | i > j     = extractNeighbors j i xs
    | otherwise = (getNeighbors i, getNeighbors j)
  where
    lastIndex = length xs - 1
    getNeighbors x
        | x == 0        = triplet lastIndex x (x+1)
        | x == lastIndex = triplet (x-1) x 0
        | otherwise     = triplet (x-1) x (x+1)
    triplet l n r = (xs !! l, xs !! n, xs !! r)

neighborDistanceDelta :: ((Int,Int,Int), (Int,Int,Int)) -> IO Float
neighborDistanceDelta ((l1,n1,r1), (l2,n2,r2))
    | n1 == l2 = do
        currentDistance <- Distancias.somaDistancias [l1,n1,n2,r2]
        newDistance     <- Distancias.somaDistancias [l1,n2,n1,r2]
        return (newDistance - currentDistance)
    | n2 == l1 = do
        currentDistance <- Distancias.somaDistancias [l2,n2,n1,r1]
        newDistance     <- Distancias.somaDistancias [l2,n1,n2,r1]
        return (newDistance - currentDistance)
    | otherwise = do
        d1 <- Distancias.somaDistancias [l1,n1,r1]
        d2 <- Distancias.somaDistancias [l2,n2,r2]
        let currentDistance = d1 + d2

        d3 <- Distancias.somaDistancias [l1,n2,r1]
        d4 <- Distancias.somaDistancias [l2,n1,r2]
        let newDistance = d3 + d4

        return (newDistance - currentDistance)

swapNodes :: Int -> Int -> [a] -> IO [a]
swapNodes i j xs
    | i == j    = return xs
    | i > j     = swapNodes j i xs
    | otherwise = return swapped
  where
    swapped = take i xs ++ 
              [xs !! j] ++ 
              take (j - i - 1) (drop (i+1) xs) ++ 
              [xs !! i] ++ 
              drop (j+1) xs

findFirstImprovement :: [ (Int, Int) ] -> [Int] -> IO (Maybe (Int, Int))
findFirstImprovement [] _ = return Nothing
findFirstImprovement ((i, j):rest) route = do
    let neighbors = extractNeighbors i j route
    delta <- neighborDistanceDelta neighbors
    if delta < 0
        then return (Just (i, j))
        else findFirstImprovement rest route