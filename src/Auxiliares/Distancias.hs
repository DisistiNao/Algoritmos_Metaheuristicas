module Auxiliares.Distancias where

import qualified Auxiliares.LerArquivo as LerArquivo

main :: Int -> Int -> IO Float
main a b = do
    contents <- LerArquivo.readInput
    let listCoordenadas = LerArquivo.coordenadas a b contents
    return (calcularDistancia listCoordenadas)

calcularDistancia :: [Int] -> Float
calcularDistancia [x1, x2, y1, y2] =
    sqrt ((dx ^ 2) + (dy ^ 2))
  where
    dx = fromIntegral (x1 - x2)
    dy = fromIntegral (y1 - y2)

somaDistancias :: [Int] -> IO Float
somaDistancias (x:xs) = soma x (x:xs)
    where
        soma y [x] = main y x
        soma y (x:xs:xss) = do
            d <- main x xs
            rest <- soma y (xs:xss)
            return (d + rest)