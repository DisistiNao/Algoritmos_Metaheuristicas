module Main where

import qualified Construtivas.Aleatoria as Aleatoria
import qualified Construtivas.Gulosa as Gulosa
import qualified Construtivas.ParcialmenteGulosa as ParcialmenteGulosa

import qualified Auxiliares.LerArquivo as LerArquivo
import qualified Auxiliares.Distancias as Distancias

main :: IO ()
main = menu

menu :: IO ()
menu = do
    putStrLn "Digite uma opção: "
    putStrLn "1 - Construtivas"
    putStrLn "0 - Exit"
    option <- readLn
    case option of
        1 -> menuConstrutivas
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