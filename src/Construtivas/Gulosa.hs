module Construtivas.Gulosa where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.RunTimer as RunTimer

main :: IO ()
main = RunTimer.timer $ do
    contents <- selecaoGulosa [0] [1..50]
    total <- Distancias.somaDistancias contents
    ShowOrder.main contents
    putStrLn (show total)
    
selecaoGulosa :: [Int] -> [Int] -> IO [Int]
selecaoGulosa x [] = return x
selecaoGulosa (x:xs) (l:list) = do
    menor <- selectMenor x l list
    let newList = removeItem menor (l:list)
    selecaoGulosa (menor:x:xs) newList

selectMenor :: Int -> Int -> [Int] -> IO Int
selectMenor x menor [] = return menor
selectMenor x menor (l:list) = do
    esq <- Distancias.main x menor
    dir <- Distancias.main x l 
    if esq <= dir
        then selectMenor x menor list
        else selectMenor x l list

removeItem :: Int -> [Int] -> [Int]
removeItem item = filter (/= item)