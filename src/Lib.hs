module Lib
    ( processImage
    ) where

import Codec.Picture
import Data.Either
import System.Directory
import Debug.Trace
import System.IO

processImage :: String -> Double -> IO ()
processImage fname angle = do
  a <- readImage $ fname
  case a of
    Left err -> putStrLn err
    Right img -> do
      putStrLn "Image Loaded"
      let grey = pixelMap rgbToGreyscale $ convertRGB8 img
          result = decompose grey angle
          w = length $ result !! 0
          h = length result
          renderer x y = PixelYA8 (truncate $ result !! y !! x) 255
      print $ "saving " ++ show w ++ "x" ++ show h
      print "--OK"
      writePng "res/result.png" $ generateImage renderer w h
  putStrLn "someFunc: "

analyzeImage :: Image PixelRGB8 -> Int
analyzeImage img = unsafePerformIO $ do
  let w = imageWidth img
      h = imageHeight img
  putStrLn $ show w ++ "x" ++ show h
  return 1
                         
