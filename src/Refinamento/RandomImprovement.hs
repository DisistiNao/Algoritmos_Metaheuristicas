module Refinamento.RandomImprovement where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.Shuffle as Shuffle

import qualified Construtivas.Gulosa as Gulosa

import Data.List (minimumBy)
import Control.Monad (forM)

main :: IO ()
main = do
    (c:rest) <- Gulosa.selecaoGulosa [0] [1..50]
    ShowOrder.main (c:rest)
    total <- Distancias.somaDistancias (c:rest)
    putStrLn (show total)
    RunTimer.timer $ improveRoute (c:rest)

improveRoute :: [Int] -> IO ()
improveRoute route = do
    routeDistance <- Distancias.somaDistancias route
    let n = length route
    let positions = [0..n-1]
    let indexPairs = [(i, j) | i <- positions, j <- positions, i < j]

    shuffledPairs <- Shuffle.main indexPairs
    let lenList = (length shuffledPairs) `div` 5
    let first20Percent = take lenList shuffledPairs
    maybeFirstCandidate <- findFirstImprovement first20Percent route

    case maybeFirstCandidate of
        Nothing -> return ()
        Just (i, j) -> do
            newRoute <- swapNodes i j route
            ShowOrder.main newRoute
            dist <- Distancias.somaDistancias newRoute
            putStrLn (show dist)
            improveRoute newRoute

findFirstImprovement :: [ (Int, Int) ] -> [Int] -> IO (Maybe (Int, Int))
findFirstImprovement [] _ = return Nothing
findFirstImprovement ((i, j):rest) route = do
    let neighbors = extractNeighbors i j route
    delta <- neighborDistanceDelta neighbors
    if delta < 0
        then return (Just (i, j))
        else findFirstImprovement rest route


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