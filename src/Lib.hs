module Lib
    ( someFunc
    ) where

import Codec.Picture
import Data.Either
import System.Directory
import System.IO.Unsafe

someFunc :: IO ()
someFunc = do
  home <- getHomeDirectory
  a <- readImage $ home ++ "/Downloads/haskell.png"
  case a of
    Left err -> putStrLn err
    Right img -> do
      putStrLn "Image Loaded"
      print $ analyzeImage $ convertRGB8 img
  putStrLn "someFunc: "

analyzeImage :: Image PixelRGB8 -> Int
analyzeImage img = unsafePerformIO $ do
  let w = imageWidth img
      h = imageHeight img
  putStrLn $ show w ++ "x" ++ show h
  return 1
                         
