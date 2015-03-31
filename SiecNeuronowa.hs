module SiecNeuronowa where

class SiecNeuronowa siec where
  przerob
    :: siec
    -> [Double] -- wejscie
    -> [Double] -- wyjscie
  cwicz
    :: siec     -- przed treningiem
    -> [Double] -- wejscie
    -> [Double] -- oczekiwane wyjscie
    -> siec     -- po treningu
