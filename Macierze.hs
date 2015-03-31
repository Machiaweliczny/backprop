module Macierze where

import Numeric.LinearAlgebra as N

-- funkcje dla macierzy

mapujMacierz :: (Field a)
   => (a -> a)
   -> Matrix a
   -> Matrix a
mapujMacierz f m = (r N.><k) zmapowane
  where m' = toList (flatten m)
        zmapowane = map f m'
        r = rows m
        k = cols m

hadamardProduct :: (Field a)
  => Matrix a
  -> Matrix a
  -> Matrix a
hadamardProduct m1 m2 = (r N.><k) zipped
  where m1' = toList (flatten m1)
        m2' = toList (flatten m2)
        zipped = zipWith (*) m1' m2'
        r = rows m1
        k = cols m1

-- obliczenia z warstw będą jako wektory
type WektorKolumnowy a = Matrix a

listaNaWektorKolumnowy :: (Ord a, Field a)
    => [a]
    -> WektorKolumnowy a
listaNaWektorKolumnowy x = (len N.><1 ) x
    where len = length x

wektorKolumnowyNaListe :: (Ord a, Field a)
    => WektorKolumnowy a
    -> [a]
wektorKolumnowyNaListe = toList . flatten
