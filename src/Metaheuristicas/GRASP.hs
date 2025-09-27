module Metaheuristicas.GRASP where

import System.Random (randomRIO)

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.RunTimer as RunTimer

import qualified Construtivas.Gulosa as Gulosa

import qualified Refinamento.FirstImprovement as FirstImprovement

main :: IO ()
main = RunTimer.timer $ do
    contents <- Gulosa.selecaoGulosa [0] [1..50]
    best <- FirstImprovement.best contents
    bestValue <- Distancias.somaDistancias best
    
    bestGRASP <- runGRASP best bestValue 0
    bestGRASPValue <- Distancias.somaDistancias bestGRASP
    ShowOrder.main bestGRASP
    putStrLn (show bestGRASPValue)

runGRASP :: [Int] -> Float -> Int -> IO [Int]
runGRASP best bestValue count
    | count > 50 = return best
    | otherwise = do
        contents <- construct 0.1 [0] [1..50]
        local <- FirstImprovement.best contents
        localValue <- Distancias.somaDistancias local
        ShowOrder.main local
        putStrLn (show localValue)
        if bestValue > localValue
            then runGRASP local localValue 0
            else runGRASP best bestValue (count+1)

construct :: Float -> [Int] -> [Int] -> IO [Int]
construct _ x [] = return x
construct alpha (x:xs) list = do
    allNeighbors <- findNeighbors x list []
    let ordedNeighbors = mergeSort allNeighbors
    
    let (_, gMin) = firstElement ordedNeighbors
    let (_, gMax) = lastElement ordedNeighbors
    
    let minValue = gMin + alpha * (gMax - gMin)
    
    let filtedList = filter (\(_, value) -> value <= minValue) ordedNeighbors
    (selected, _) <- randomElement filtedList

    construct alpha (selected : x : xs) (remove selected list)
    where
        findNeighbors :: Int -> [Int] -> [(Int, Float)] -> IO [(Int, Float)]
        findNeighbors x [] list = return list
        findNeighbors x (n:neighbors) list = do
            nDist <- Distancias.main x n
            findNeighbors x neighbors ((n, nDist):list)
        firstElement :: [a] -> a
        firstElement (x:_) = x

        lastElement :: [a] -> a
        lastElement xs = last xs

        randomElement :: [a] -> IO a
        randomElement xs = do
            idx <- randomRIO (0, length xs - 1)
            return (xs !! idx)

        merge :: [(Int, Float)] -> [(Int, Float)] -> [(Int, Float)]
        merge [] ys = ys
        merge xs [] = xs
        merge ((x0, x):xs) ((y0, y):ys)
            | x <= y    = (x0, x) : merge xs ((y0, y):ys)
            | otherwise = (y0, y) : merge ((x0, x):xs) ys

        splitHalf :: [(Int, Float)] -> ([(Int, Float)],[(Int, Float)])
        splitHalf xs = (take half xs, drop half xs)
            where half = length xs `div` 2

        mergeSort :: [(Int, Float)] -> [(Int, Float)]
        mergeSort [] = []
        mergeSort [x] = [x]
        mergeSort xs = merge (mergeSort left) (mergeSort right)
            where (left, right) = splitHalf xs
        
        remove :: Eq a => a -> [a] -> [a]
        remove _ [] = []
        remove y (z:zs)
            | y == z    = zs
            | otherwise = z : remove y zs