module Auxiliares.LerArquivo where

import System.IO
import Control.Exception (evaluate)

readInput :: IO [(Int, Int, Int)]
readInput = withFile "data/C50.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    _ <- evaluate (length contents)
    return (parseInput contents)

parseInput :: String -> [(Int, Int, Int)]
parseInput = map parseLine . lines
  where
    parseLine line =
        case map read (words line) of
            [i, x, y] -> (i, x, y)
            _         -> error $ "Linha inválida: " ++ line

coordenadas :: Int -> Int -> [(Int, Int, Int)] -> [Int]
coordenadas id1 id2 triples =
    let (Just (_,x1,y1)) = lookupId id1 triples
        (Just (_,x2,y2)) = lookupId id2 triples
    in [x1, x2, y1, y2]

lookupId :: Int -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
lookupId _ [] = Nothing
lookupId n ((i,x,y):rest)
    | n == i    = Just (i,x,y)
    | otherwise = lookupId n rest
