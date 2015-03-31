module Cwicz where

import Mnist
import SiecNeuronowa
import PropagacjaWsteczna

import Data.Maybe
import Data.List

oczekiwane :: [[Double]] -- wyjscie
oczekiwane =
    [
        [0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9]
    ]

type OznaczonyObrazek = ([Double], Int)

cwiczWzorzec
  :: (SiecNeuronowa n)
  => OznaczonyObrazek
  -> n
  -> n
cwiczWzorzec daneTreningowe siec = cwicz siec dane oczekiwanyRozklad
  where dane = fst daneTreningowe
        cyfra = snd daneTreningowe
        oczekiwanyRozklad = oczekiwane !! cyfra

przecwiczWszystkieWzorce
  :: (SiecNeuronowa n)
  => n
  -> [OznaczonyObrazek]
  -> n
przecwiczWszystkieWzorce = foldr cwiczWzorzec

takieSame :: (Eq a) => a -> a -> Int
takieSame x y =
  if x == y
  then 1
  else 0

interpretuj :: [Double] -> Int
interpretuj v = fromJust (elemIndex (maximum v) v)

ocenWzorzec
  :: (SiecNeuronowa n)
  => n
  -> OznaczonyObrazek
  -> Int
ocenWzorzec siec oznaczone = takieSame zgadnietaCyfra poprawnaCyfra
  where dane = fst oznaczone
        poprawnaCyfra = snd oznaczone
        daneWynikowe = przerob siec dane
        zgadnietaCyfra = interpretuj daneWynikowe

ocenWszystkieWzorce
  :: (SiecNeuronowa n)
  => n
  -> [OznaczonyObrazek]
  -> [Int]
ocenWszystkieWzorce = map . ocenWzorzec

wczytajDaneTreningowe :: IO [OznaczonyObrazek]
wczytajDaneTreningowe = do
  etykiety <- wczytajEtykiety "mnist/train-labels-idx1-ubyte"
  obrazki <- wczytajObrazki "mnist/train-images-idx3-ubyte"
  return (zip (map znormalizowanePiksele obrazki) etykiety)

wczytajDaneTestowe :: IO [OznaczonyObrazek]
wczytajDaneTestowe = do
  etykiety <- wczytajEtykiety "mnist/t10k-labels-idx1-ubyte"
  obrazki <- wczytajObrazki "mnist/t10k-images-idx3-ubyte"
  return (zip (map znormalizowanePiksele obrazki) etykiety)
