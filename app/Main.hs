module Main where

import Lib
import System.IO
import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    let fname = head args
        n = read $ args !! 1 :: Int
    processImage2 fname n
