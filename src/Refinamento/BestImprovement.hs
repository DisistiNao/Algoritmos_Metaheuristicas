module Refinamento.BestImprovement where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.RunTimer as RunTimer
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.ImprovementUtils as ImprovementUtils

import Data.List (minimumBy)
import Control.Monad (forM)

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

    candidates <- forM indexPairs $ \(i, j) -> do
        let neighbors = ImprovementUtils.extractNeighbors i j route
        delta <- ImprovementUtils.neighborDistanceDelta neighbors
        return (delta, (i, j))

    let bestCandidate = minimumBy (\(d1,_) (d2,_) -> compare d1 d2) candidates
    let (bestDelta, (i, j)) = bestCandidate

    if bestDelta < 0
        then do
            newRoute <- ImprovementUtils.swapNodes i j route
            ShowOrder.main newRoute
            dist <- Distancias.somaDistancias newRoute
            putStrLn (show dist)
            improveRoute newRoute
        else return ()