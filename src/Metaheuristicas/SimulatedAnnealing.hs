module Metaheuristicas.SimulatedAnnealing where

import System.Random

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ImprovementUtils as ImprovementUtils
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder

import qualified Construtivas.Aleatoria as Aleatoria

import qualified Refinamento.FirstImprovement as FirstImprovement

main :: IO ()
main = RunTimer.timer $ do
    annealingList <- annealing
    total <- Distancias.somaDistancias annealingList
    ShowOrder.main annealingList
    putStrLn (show total)

annealing :: IO [Int]
annealing = do
    local <- Aleatoria.permute [0..50]
    temp <- initialTemp local 1.1 0.95 500 100
    putStrLn ("Temperatura Máxima: " ++ show temp)
    global <- FirstImprovement.returnImproveRoute local
    loop1 25 local global temp
    return global

loop1 :: Int -> [Int] -> [Int] -> Int -> IO [Int]
loop1 sAmax local global temp
    | fromIntegral temp > 0.1 = loop2 0 0.9 sAmax local global temp
    | otherwise = return global

loop2 :: Int -> Float -> Int -> [Int] -> [Int] -> Int -> IO [Int]
loop2 iterT alpha sAmax local global temp
    | iterT < sAmax = do
        let iterT' = iterT + 1
        
        i <- randomRIO (0, 50) :: IO Int
        j' <- randomRIO (0, 50) :: IO Int
        
        let j = if i == j'
            then (j' + 1) `mod` 51 
            else j'

        newList <- ImprovementUtils.swapNodes i j local
        newListValue <- Distancias.somaDistancias newList
        
        localValue <- Distancias.somaDistancias local
        let delta = newListValue - localValue

        if delta < 0
            then do
                globalValue <- Distancias.somaDistancias global
                refined <- FirstImprovement.returnImproveRoute newList
                refinedValue <- Distancias.somaDistancias refined
                
                if refinedValue < globalValue
                    then do
                        loop2 iterT' alpha sAmax newList refined temp
                    else loop2 iterT' alpha sAmax newList global temp
            else do
                randomDouble <- randomIO :: IO Double

                if randomDouble < euler ** (- realToFrac delta / fromIntegral temp)
                    then loop2 iterT' alpha sAmax newList global temp
                    else loop2 iterT' alpha sAmax local global temp
    | otherwise = do
        let temp' = round (alpha * fromIntegral temp) - 1
        putStrLn ("Temperatura atual: " ++ show temp')
        ShowOrder.main global
        globalValue <- Distancias.somaDistancias global
        putStrLn ("Melhor fo: " ++ show globalValue)
        loop1 sAmax local global temp'

initialTemp :: [Int] -> Float -> Float -> Int -> Int -> IO Int
initialTemp list beta gamma sAmax temp = do
    acceptedNeighbors <- countAcceptedNeighbors 0 sAmax
    if fromIntegral acceptedNeighbors >= fromIntegral sAmax * gamma
        then return temp
        else do
            let temp' = round (fromIntegral temp * beta)
            initialTemp list beta gamma sAmax temp'
  where
    countAcceptedNeighbors :: Int -> Int -> IO Int
    countAcceptedNeighbors count time
        | time <= 0 = return count
        | otherwise = do
            i <- randomRIO (0, 50) :: IO Int
            j' <- randomRIO (0, 50) :: IO Int
            
            let j = if i == j'
                    then (j' + 1) `mod` 51
                    else j'

            newList <- ImprovementUtils.swapNodes i j list
            newListValue <- Distancias.somaDistancias newList
            let delta = newListValue - fromIntegral temp

            if delta < 0
                then countAcceptedNeighbors (count+1) (time-1)
                else do
                    randomDouble <- randomIO :: IO Double
                    if randomDouble < euler ** (- realToFrac delta / fromIntegral temp)
                        then countAcceptedNeighbors (count+1) (time-1)
                        else countAcceptedNeighbors count (time-1)

-- Euler number
fact :: Int -> Integer
fact n = product [1..fromIntegral n]

calculateE :: Int -> Double
calculateE numTerms = sum [1.0 / fromIntegral (fact k) | k <- [0..numTerms-1]]

euler :: Double
euler = calculateE 20