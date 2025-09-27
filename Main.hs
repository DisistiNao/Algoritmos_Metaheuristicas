module Main where

import qualified Construtivas.Aleatoria as Aleatoria
import qualified Construtivas.Gulosa as Gulosa
import qualified Construtivas.ParcialmenteGulosa as ParcialmenteGulosa

import qualified Metaheuristicas.GRASP as GRASP
import qualified Metaheuristicas.MultiStart as MultiStart
import qualified Metaheuristicas.SimulatedAnnealing as SimulatedAnnealing

import qualified Refinamento.BestImprovement as BestImprovement
import qualified Refinamento.FirstImprovement as FirstImprovement
import qualified Refinamento.RandomImprovement as RandomImprovement

main :: IO ()
main = menu

menu :: IO ()
menu = do
    putStrLn "Digite uma opção: "
    putStrLn "1 - Construtivas"
    putStrLn "2 - Refinamento"
    putStrLn "3 - Metaheuristicas"
    putStrLn "0 - Exit"
    option <- readLn
    case option of
        1 -> menuConstrutivas
        2 -> menuRefinamento
        3 -> menuMetaheuristicas
        0 -> putStrLn ""
        _ -> putStrLn "Error" >> menu


menuConstrutivas :: IO ()
menuConstrutivas = do
    putStrLn "Digite uma opção: "
    putStrLn "1 - Aleatoria"
    putStrLn "2 - Gulosa / Vizinho mais próximo"
    putStrLn "3 - Parcialmente Gulosa"
    putStrLn "0 - Exit"
    option <- readLn
    case option of
        1 -> Aleatoria.main >> menu
        2 -> Gulosa.main >> menu
        3 -> ParcialmenteGulosa.main >> menu
        0 -> menu
        _ -> putStrLn "Error" >> menuConstrutivas

menuRefinamento :: IO ()
menuRefinamento = do
    putStrLn "Digite uma opção: "
    putStrLn "1 - Best Improvement"
    putStrLn "2 - First Improvement"
    putStrLn "3 - Random Improvement"
    putStrLn "0 - Exit"
    option <- readLn
    case option of
        1 -> BestImprovement.main >> menu
        2 -> FirstImprovement.main >> menu
        3 -> RandomImprovement.main >> menu
        0 -> menu
        _ -> putStrLn "Error" >> menuRefinamento

menuMetaheuristicas :: IO ()
menuMetaheuristicas = do
    putStrLn "Digite uma opção: "
    putStrLn "1 - Multi-Start"
    putStrLn "2 - Simulated Annealing"
    putStrLn "3 - GRASP"
    putStrLn "0 - Exit"
    option <- readLn
    case option of
        1 -> MultiStart.main >> menu
        2 -> SimulatedAnnealing.main >> menu
        3 -> GRASP.main >> menu
        0 -> menu
        _ -> putStrLn "Error" >> menuMetaheuristicas
