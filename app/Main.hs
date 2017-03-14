module Main where

import Lib
import System.IO
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    let fname = head args
        angle = read $ args !! 1 :: Double
    processImage fname angle
