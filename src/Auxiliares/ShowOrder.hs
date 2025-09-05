module Auxiliares.ShowOrder where

main :: [Int] -> IO ()
main [] = putStrLn ""
main (x:xs) = putStrLn (order (x:xs) ++ show x)
    where
        order [] = ""
        order (x:xs) = show x ++ " -> " ++ order xs
