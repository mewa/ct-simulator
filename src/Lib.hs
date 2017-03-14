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

rgbToGreyscale :: PixelRGB8 -> PixelYA8
rgbToGreyscale (PixelRGB8 r g b) = 
  let rgb = zipWith (*) [0.2126, 0.7152, 0.0722] $ map (fromIntegral) [r, g, b] 
      value = fromInteger . round . sum $ rgb :: Pixel8
  in (PixelYA8 value 255)

decompose :: Image PixelYA8 -> Double -> [[Double]]
decompose img fi = let 
    w = imageWidth img
    h = imageHeight img
    c = ceiling . sqrt . fromIntegral $ w^2 + h^2
    half = c -- ceiling $ 0.5 * fromIntegral (c - w)
    result = [scan img fi i | i <- [0..(w - 1)]]
  in result

xyValue :: Image PixelYA8 -> (Int, Int) -> PixelYA8
xyValue img (x, y) = let 
    w = imageWidth img
    h = imageHeight img
    value = if x < 0 || y < 0 || x >= w || y >= h
      then PixelYA8 0 255
      else pixelAt img x y
    in value

intersect :: Image PixelYA8 -> Double -> Int -> [(Int, Int)]
intersect img fi step = let
    d = fromIntegral :: Int -> Double
    i = truncate :: Double -> Int
    w = d $ imageWidth img
    h = d $ imageHeight img

    c = sqrt $ w^2 + h^2
    dc = c / 2

    angle = fi * pi / 180

    fgen theta = if mod (round $ theta / (pi / 2)) 2 == 1
      then (0, \x -> (d step - x * cos theta) / sin theta)
      else (1, \y -> (d step - y * sin theta) / cos theta)
    (m, f) = fgen angle
    (xs, ys) = if m == 0
      then (
        fmap (i . f . d) ys, -- xs
        [0..(i h - 1)] -- ys
        )
      else (
        [0..(i w - 1)], -- xs
        fmap (i . f . d) xs --ys
        )
    result = zip xs ys
  in --trace ("xs " ++ show m ++ "m: " ++ show result ++ " step " ++ show step ++ "\n") 
    --trace ("step: " ++ show step)
  result

scan :: Image PixelYA8 -> Double -> Int -> [Double]
scan img fi step = let 
    w = imageWidth img
    h = imageHeight img
    angle = fi * pi / 180.0
    f = (round . \x -> tan angle * fromIntegral x) :: Int -> Int
    xys = intersect img fi step
    pixels = map (xyValue img) xys
    result = map (\(PixelYA8 y a) -> y) pixels
  in 
   --trace ("result[" ++ (show $ length result) ++ "]: (fi " ++ show fi ++ ")\n" ++ show result) $ 
  map fromIntegral result                         
