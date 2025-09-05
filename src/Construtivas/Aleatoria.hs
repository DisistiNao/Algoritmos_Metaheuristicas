module Construtivas.Aleatoria where

import qualified Auxiliares.Distancias as Distancias
import qualified Auxiliares.ShowOrder as ShowOrder
import qualified Auxiliares.RunTimer as RunTimer

import System.Random (randomRIO)

main :: IO ()
main = RunTimer.timer $ do
    contents <- permute [0..50]
    total <- Distancias.somaDistancias contents
    ShowOrder.main contents
    putStrLn (show total)

permute :: [Int] -> IO [Int]
permute [] = return [] 
permute xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, x:right) = splitAt i xs
    rest <- permute (left ++ right)
    return (x : rest)