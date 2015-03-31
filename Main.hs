import PropagacjaWsteczna
import Cwicz
import Obrazki
import SiecNeuronowa

import Numeric.LinearAlgebra
import Data.List
import System.Random
import System.IO

losowe :: Int -> [Double]
losowe seed = map (/100) (randoms (mkStdGen seed))

losoweWagiNaPoczatek :: Int -> Int -> Int -> Matrix Double
losoweWagiNaPoczatek iloscWejsc iloscWyjsc seed = (iloscWyjsc><iloscWejsc) wagi
    where wagi = take (iloscWyjsc*iloscWejsc) (losowe seed)

wczytajInt :: IO Int
wczytajInt = getLine >>= return . read

sprawdzTreningowe :: Int -> Int
sprawdzTreningowe n = if n > 999 && n <= 60000 then n else 10000

sprawdzTestowe :: Int -> Int
sprawdzTestowe n = if n > 99 && n <= 10000 then n else 1000

szybkoscNauki = 0.015

main :: IO ()
main = do
  let w1 = losoweWagiNaPoczatek (28*28 + 1) 20 7
  let w2 = losoweWagiNaPoczatek 20 10 77
  let poczatkowaSiec = zbudujSiecBP szybkoscNauki [w1, w2] tanhFA
  daneTreningowe60k <- wczytajDaneTreningowe
  putStrLn $ "Szybkość nauki: " ++ show szybkoscNauki
  putStrLn $ "Na jakiej ilości trenować[1k-60k]:"
  iloscTreningowych <- wczytajInt
  let daneTreningowe = take (sprawdzTreningowe iloscTreningowych) daneTreningowe60k
  putStrLn $ "Trenuje z użyciem " ++ show (length daneTreningowe) ++ " obrazków"
  let wyuczonaSiec = przecwiczWszystkieWzorce poczatkowaSiec daneTreningowe
  daneTestowe10k <- wczytajDaneTestowe
  putStrLn $ "Na jakiej ilości testować[1k-10k]:"
  iloscTestowych <- wczytajInt
  let daneTestowe = take (sprawdzTestowe iloscTestowych) daneTestowe10k
  putStrLn $ "Testuje z użyciem " ++ show (length daneTestowe) ++ " obrazków"
  let wyniki = ocenWszystkieWzorce wyuczonaSiec daneTestowe
  let poprawnie = fromIntegral (sum wyniki)
  let wszystkich = fromIntegral iloscTestowych
  let procentowo = 100.0 * poprawnie / wszystkich
  putStrLn $ "Odgadłem " ++ show procentowo ++ "% testowych danych."
  putStrLn $ "Teraz podaj jakieś obrazki(jpg) do odgadnięcia."
  zgadujCyfre wyuczonaSiec

zgadujCyfre wyuczonaSiec = do
  putStrLn "Podaj jpg:"
  src <- getLine
  znormalizowanePiksele <-  wczytajJpg src
  let cyfra = interpretuj $ przerob wyuczonaSiec znormalizowanePiksele
  putStrLn $ "Zgaduje ze to " ++ show cyfra
  zgadujCyfre wyuczonaSiec
