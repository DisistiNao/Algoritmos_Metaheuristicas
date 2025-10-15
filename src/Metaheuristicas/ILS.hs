module Metaheuristicas.ILS where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ImprovementUtils as ImprovementUtils
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.Shuffle as Shuffle

import qualified Construtivas.Gulosa as Gulosa

import qualified Refinamento.FirstImprovement as FirstImprovement

import Control.Monad (foldM)

main :: IO ()
main = do
    putStrLn "Digite o número de niveis sem melhora: "
    intInputStr <- getLine
    let ilsMax = read intInputStr :: Int
    run ilsMax

run :: Int -> IO ()
run ilsMax = RunTimer.timer $ do
    initialList <- Gulosa.selecaoGulosa [0] [1..50]
    list <- FirstImprovement.best initialList
    listValue <- Distancias.somaDistancias list
    
    ShowOrder.main list
    putStrLn (show listValue)

    iterations list listValue 0 0 1 ilsMax

iterations :: [Int] -> Float -> Int -> Int -> Int -> Int -> IO ()
iterations best bestValue iter iterPerturbation level ilsMax
    | iter < ilsMax = do
        list <- perturbation best level
        listValue <- Distancias.somaDistancias list
        if listValue < bestValue
            then do
                ShowOrder.main list
                putStrLn (show listValue)
                iterations list listValue 0 0 1 ilsMax
            else if iterPerturbation < ilsMax
                then iterations best bestValue iter (iterPerturbation+1) level ilsMax
                else iterations best bestValue (iter+1) 0 (level+1) ilsMax
    | otherwise = return ()

perturbation :: [Int] -> Int -> IO [Int]
perturbation list level = do
    let n = length list
    let positions = [0..n-1]
    let indexPairs = [(i, j) | i <- positions, j <- positions, i < j]
    shuffledPairs <- Shuffle.main indexPairs
    let perturbations = take level shuffledPairs

    listPermutation <- foldM (\acc (i,j) -> ImprovementUtils.swapNodes i j acc) list perturbations
    newList <- FirstImprovement.best listPermutation

    return newList