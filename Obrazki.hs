module Obrazki(wczytajJpg) where

import Graphics.GD

wczytajJpg :: String -> IO [Double]
wczytajJpg src = do
  plik <- loadJpegFile src
  thumb <- resizeImage 28 28 plik
  piksele <- pullIO [getPixel ((p `rem` 28), (p `div` 28)) thumb | p <- [0..(28*28-1)]]
  return $ adjustColors $ map grayScale piksele

pullIO :: [IO a] -> IO [a]
pullIO [] = return []
pullIO (x:xs) = do
  ogon <- pullIO xs
  nx <- x
  return $ nx:ogon

grayScale:: Color -> Double
grayScale color = let (r,g,b,_) = toRGBA color in 0.2989*(fromIntegral r / 255.0)+0.5870*(fromIntegral g / 255.0)+0.1140*(fromIntegral b / 255.0)

adjustColors :: [Double] -> [Double]
adjustColors colors = if black > white then colors else reverseColors colors
                      where
                      black = length [c | c <- colors, c < 0.5]
                      white = length colors - black

reverseColors :: [Double] -> [Double]
reverseColors colors = map (\x -> 1.0 - x) colors
