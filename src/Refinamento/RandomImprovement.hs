module Refinamento.RandomImprovement where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.Shuffle as Shuffle
import qualified Auxiliares.ImprovementUtils as ImprovementUtils

main :: IO ()
main = do
    (c:rest) <- ImprovementUtils.initialization
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
    maybeFirstCandidate <- ImprovementUtils.findFirstImprovement first20Percent route

    case maybeFirstCandidate of
        Nothing -> return ()
        Just (i, j) -> do
            newRoute <- ImprovementUtils.swapNodes i j route
            ShowOrder.main newRoute
            dist <- Distancias.somaDistancias newRoute
            putStrLn (show dist)
            improveRoute newRoute