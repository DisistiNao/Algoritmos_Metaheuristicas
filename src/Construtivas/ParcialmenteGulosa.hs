module Construtivas.ParcialmenteGulosa where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.RunTimer as RunTimer

import System.Random (randomRIO)

main :: IO ()
main = RunTimer.timer $ do
    contents <- selecaoParcial [0] [1..50]
    total <- Distancias.somaDistancias contents
    ShowOrder.main contents
    putStrLn (show total)

selecaoParcial :: [Int] -> [Int] -> IO [Int]
selecaoParcial x [] = return x
selecaoParcial (x:xs) (l:list) = do
    menor <- selectMenor x [l] list
    let newList = removeItem menor (l:list)
    selecaoParcial (menor:x:xs) newList

selectMenor :: Int -> [Int] -> [Int] -> IO Int
selectMenor x menores [] = pickRandomElement menores
selectMenor x (m:menores) (l:list) = do
    esq <- Distancias.main x m
    dir <- Distancias.main x l 
    if esq <= dir
        then selectMenor x (m:menores) list
        else selectMenor x (l:m:menores) list

pickRandomElement :: [a] -> IO a
pickRandomElement xs = do
  randomIndex <- randomRIO (0, length xs - 1)
  return (xs !! randomIndex)

removeItem :: Int -> [Int] -> [Int]
removeItem item = filter (/= item)