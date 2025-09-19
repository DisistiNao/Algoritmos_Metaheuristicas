module Metaheuristicas.MultiStart where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder

import qualified Construtivas.Aleatoria as Aleatoria
import qualified Construtivas.Gulosa as Gulosa

import qualified Refinamento.FirstImprovement as FirstImprovement

main :: IO ()
main = RunTimer.timer $ do
    bestList <- Gulosa.selecaoGulosa [0] [1..50]
    selectBestLocal bestList 10

selectBestLocal :: [Int] -> Int -> IO ()
selectBestLocal bestList count
    | count <= 0 = do
        total <- Distancias.somaDistancias bestList
        ShowOrder.main bestList
        putStrLn (show total)
    | otherwise = do
        list <- Aleatoria.permute [0..50]
        newList <- FirstImprovement.best list

        bestDist <- Distancias.somaDistancias bestList
        newDist <- Distancias.somaDistancias newList

        ShowOrder.main newList
        dist <- Distancias.somaDistancias newList
        putStrLn (show dist)
        putStrLn (show count)

        if newDist < bestDist
            then selectBestLocal newList 10
            else selectBestLocal bestList (count-1)