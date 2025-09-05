module Auxiliares.RunTimer where

import System.CPUTime
import Numeric (showFFloat)

timer :: IO a -> IO a
timer action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12) :: Double
    putStrLn $ "Execution time: " ++ showFFloat (Just 5) diff " seconds"
    return result