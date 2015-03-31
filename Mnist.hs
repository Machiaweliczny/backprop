module Mnist (Obrazek(..), znormalizowanePiksele, wczytajObrazki, wczytajEtykiety, zamienNaMacierz)
  where

import Data.Word
import Data.Binary.Get
import qualified Data.List.Split as S
import qualified Data.ByteString.Lazy as BL
import Numeric.LinearAlgebra

data Obrazek = Obrazek {
      oWysokosc :: Int
    , oSzerokosc :: Int
    , oPiksele :: [Word8]
    } deriving (Eq, Show)

zamienNaMacierz :: Obrazek -> Matrix Double
zamienNaMacierz obrazek = (rzedy><columny) piksele :: Matrix Double
  where rzedy = oWysokosc obrazek
        columny = oSzerokosc obrazek
        piksele = map fromIntegral (oPiksele obrazek)

znormalizowanePiksele :: Obrazek -> [Double]
znormalizowanePiksele obrazek = map normalizujPiksel (oPiksele obrazek)

normalizujPiksel :: Word8 -> Double
normalizujPiksel p = (fromIntegral p) / 255.0

-- format danych MNIST dla etykiet z (http://yann.lecun.com/exdb/mnist/)
--
-- [offset] [type]          [value]          [description]
-- 0000     32 bit integer  0x00000801(2049) magic number (MSB first)
-- 0004     32 bit integer  10000            number of items
-- 0008     unsigned byte   ??               label
-- 0009     unsigned byte   ??               label
-- ........
-- xxxx     unsigned byte   ??               label
--
-- The labels values are 0 to 9.

odkodujEtykiety :: Get (Word32, Word32, [Word8])
odkodujEtykiety = do
  magicznaLiczba <- getWord32be
  iloscCyfr <- getWord32be
  bajty <- getRemainingLazyByteString
  let cyfry = BL.unpack bajty
  return (magicznaLiczba, iloscCyfr, cyfry)

-- zwraca wszystkie przeczytane cyfry
wczytajEtykiety :: FilePath -> IO [Int]
wczytajEtykiety sciezka = do
  dane <- BL.readFile sciezka
  let (_, _, cyfry) = runGet odkodujEtykiety dane
  return (map fromIntegral cyfry)

-- format Obrazkow
--
-- [offset] [type]          [value]          [description]
-- 0000     32 bit integer  0x00000803(2051) magic number
-- 0004     32 bit integer  ??               number of images
-- 0008     32 bit integer  28               number of rows
-- 0012     32 bit integer  28               number of columns
-- 0016     unsigned byte   ??               pixel
-- 0017     unsigned byte   ??               pixel
-- ........
-- xxxx     unsigned byte   ??               pixel
--
-- Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 255
-- means foreground (black).

odkodujNaglowek :: Get (Word32, Word32, Word32, Word32, [[Word8]])
odkodujNaglowek = do
  magicznaLiczba <- getWord32be
  iloscObrazkow <- getWord32be
  w <- getWord32be
  h <- getWord32be
  piksele <- getRemainingLazyByteString
  let pikseliNaObrazek = fromIntegral (w * h)
  let pikseleObrazkow = S.chunksOf pikseliNaObrazek (BL.unpack piksele)
  return (magicznaLiczba, iloscObrazkow, h, w, pikseleObrazkow)

wczytajObrazki :: FilePath -> IO [Obrazek]
wczytajObrazki sciezka = do
  dane <- BL.readFile sciezka
  let (_, _, h, w, pikseleObrazkow) = runGet odkodujNaglowek dane
  return (map (Obrazek (fromIntegral h) (fromIntegral w)) pikseleObrazkow)
