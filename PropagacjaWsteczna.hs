module PropagacjaWsteczna
  (SiecBP, zbudujSiecBP, tanhFA, identycznoscFA)
where

import Macierze
import Numeric.LinearAlgebra as N
import SiecNeuronowa

data Warstwa = Warstwa
    {
      wW :: Matrix Double,
      wFA :: FunkcjaAktywujaca
    }

instance Show Warstwa where
    show warstwa = "wagi=" ++ show (wW warstwa) ++ ", fA=" ++ show (wFA warstwa)

iloscKolumn :: Warstwa -> Int
iloscKolumn = cols . wW

data WarstwaPoPropagacji
    = WarstwaPoPropagacji
        {
          pWe :: WektorKolumnowy Double,
          pWy :: WektorKolumnowy Double,
          pF'a :: WektorKolumnowy Double,
          pW :: Matrix Double,
          pFA :: FunkcjaAktywujaca
        }
    | WejsciowaPoPropagacji
        {
          pWy :: WektorKolumnowy Double
        }

instance Show WarstwaPoPropagacji where
    show (WarstwaPoPropagacji we wy f'a wagi fA) =
        "wejscie=" ++ show we
        ++ ", wyjscie=" ++ show wy
        ++ ", f'(a)=" ++ show f'a
        ++ ", wagi=" ++ show wagi
        ++ ", fA" ++ show fA
    show (WejsciowaPoPropagacji wy) = "wyjscie=" ++ show wy

propaguj :: WarstwaPoPropagacji -> Warstwa -> WarstwaPoPropagacji
propaguj warstwaJ warstwaK = WarstwaPoPropagacji
        {
          pWe = x,
          pWy = y,
          pF'a = f'a,
          pW = w,
          pFA = wFA warstwaK
        }
  where x = pWy warstwaJ
        w = wW warstwaK
        a = w <> x
        f = asF ( wFA warstwaK )
        y = mapujMacierz f a
        f' = asF' ( wFA warstwaK )
        f'a = mapujMacierz f' a

data WarstwaPoPowrocie = WarstwaPoPowrocie
    {
      bpNabla :: WektorKolumnowy Double,
      bpBlad :: WektorKolumnowy Double,
      bpF'a :: WektorKolumnowy Double,
      bpWe :: WektorKolumnowy Double,
      bpWy :: WektorKolumnowy Double,
      bpW :: Matrix Double,
      bpFA :: FunkcjaAktywujaca
    }

instance Show WarstwaPoPowrocie where
    show warstwa =
        "nabla=" ++ show (bpNabla warstwa)
        ++ ", blad=" ++ show (bpBlad warstwa)
        ++ ", we=" ++ show (bpWe warstwa)
        ++ ", wy=" ++ show (bpWy warstwa)
        ++ ", wagi=" ++ show (bpW warstwa)

propagujWsteczOstatniaWarstwe ::
    WarstwaPoPropagacji -> WektorKolumnowy Double -> WarstwaPoPowrocie
propagujWsteczOstatniaWarstwe warstwa wektor =
    WarstwaPoPowrocie
      {
        bpNabla = nabla,
        bpBlad = ocenBlad nabla f'a (pWe warstwa),
        bpF'a = pF'a warstwa,
        bpWe = pWe warstwa,
        bpWy = pWy warstwa,
        bpW = pW warstwa,
        bpFA = pFA warstwa
      }
      where nabla =  pWy warstwa - wektor
            f'a = pF'a warstwa

ocenBlad :: WektorKolumnowy Double ->
            WektorKolumnowy Double ->
            WektorKolumnowy Double ->
            WektorKolumnowy Double
ocenBlad nabla f'a wejscie = (nabla * f'a) <> trans wejscie

propagujWstecz :: WarstwaPoPropagacji -> WarstwaPoPowrocie -> WarstwaPoPowrocie
propagujWstecz warstwaJ warstwaK =
    WarstwaPoPowrocie
      {
        bpNabla = nablaJ,
        bpBlad = ocenBlad nablaJ f'aJ (pWe warstwaJ),
        bpF'a = pF'a warstwaJ,
        bpWe = pWe warstwaJ,
        bpWy = pWy warstwaJ,
        bpW = pW warstwaJ,
        bpFA = pFA warstwaJ
      }
      where nablaJ = wKT <> (nablaK * f'aK)
            nablaK = bpNabla warstwaK
            wKT = trans ( bpW warstwaK )
            f'aK = bpF'a warstwaK
            f'aJ = pF'a warstwaJ

uaktualnijWagi :: Double -> WarstwaPoPowrocie -> Warstwa
uaktualnijWagi tempo warstwa = Warstwa
        {
          wW = noweWagi,
          wFA = bpFA warstwa
        }
    where stareWagi = bpW warstwa
          poprawka = tempo `scale` bpBlad warstwa
          noweWagi = stareWagi - poprawka

data SiecBP = SiecBP
    {
      warstwy :: [Warstwa],
      tempoNauki :: Double
    } deriving Show

zbudujSiecBP ::
  -- tempo nauki
  Double ->
  -- wagi dla warstw
  [Matrix Double] ->
  FunkcjaAktywujaca ->
  SiecBP
zbudujSiecBP tempo wagi funkcjaAktywujaca = SiecBP { warstwy=wwy, tempoNauki=tempo }
  where sprawdzoneWagi = scanl1 sprawdzWymiary wagi
        zbudujWarstwe w = Warstwa { wW=w, wFA=funkcjaAktywujaca }
        wwy = map zbudujWarstwe sprawdzoneWagi

sprawdzWymiary :: Matrix Double -> Matrix Double -> Matrix Double
sprawdzWymiary w1 w2 =
  if rows w1 == cols w2
       then w2
       else error "Wymiary macierzy się nie zgadzaja!"

propagujSiec :: WektorKolumnowy Double -> SiecBP -> [WarstwaPoPropagacji]
propagujSiec wejscie siec = tail obliczenia
  where obliczenia = scanl propaguj warstwa0 (warstwy siec)
        warstwa0 = WejsciowaPoPropagacji{ pWy=sprawdzone }
        sprawdzone = sprawdzWejscie siec wejscie

sprawdzWejscie :: SiecBP -> WektorKolumnowy Double -> WektorKolumnowy Double
sprawdzWejscie siec = sprawdzWejscioweWartosci . sprawdzWymiaryWejscia siec

sprawdzWymiaryWejscia ::
    SiecBP ->
    WektorKolumnowy Double ->
    WektorKolumnowy Double
sprawdzWymiaryWejscia siec wejscie =
  if otrzymane == oczekiwane
       then wejscie
       else error ("Wzorzec wejsciowy ma długosc" ++ show otrzymane ++ ", a powinien mieć " ++ show oczekiwane)
           where otrzymane = rows wejscie
                 oczekiwane = iloscKolumn (head (warstwy siec))

sprawdzWejscioweWartosci :: WektorKolumnowy Double -> WektorKolumnowy Double
sprawdzWejscioweWartosci wejscie =
  if (min >= 0) && (max <= 1)
       then wejscie
       else error "Wejscie jest nieunormowane, oczekuje [0,1]"
       where min = minimum wartosci
             max = maximum wartosci
             wartosci = toList ( flatten wejscie )

propagujSiecWstecz :: WektorKolumnowy Double -> [WarstwaPoPropagacji] -> [WarstwaPoPowrocie]
propagujSiecWstecz oczekiwane warstwy = scanr propagujWstecz ostatniaWarstwa ukryteWarstwy
  where ukryteWarstwy = init warstwy
        ostatniaWarstwa = propagujWsteczOstatniaWarstwe (last warstwy) oczekiwane

instance SiecNeuronowa SiecBP where
  przerob = przerobSBP
  cwicz = cwiczSBP

przerobSBP :: SiecBP -> [Double] -> [Double]
przerobSBP siec dane = wektorKolumnowyNaListe( pWy ( last wynik ))
  where wynik = propagujSiec wektor siec
        wektor = listaNaWektorKolumnowy (1:dane)

cwiczSBP :: SiecBP -> [Double] -> [Double] -> SiecBP
cwiczSBP siec dane oczekiwane = SiecBP { warstwy=noweWarstwy, tempoNauki=tempo }
  where noweWarstwy = map (uaktualnijWagi tempo) propagujWarstwyWstecz
        tempo = tempoNauki siec
        propagujWarstwyWstecz = propagujSiecWstecz (listaNaWektorKolumnowy oczekiwane) propagujWarstwy
        propagujWarstwy = propagujSiec wektor siec
        wektor = listaNaWektorKolumnowy (1:dane)

data FunkcjaAktywujaca = FunkcjaAktywujaca
    {
      asF :: Double -> Double,
      asF' :: Double -> Double,
      opis :: String
    }

instance Show FunkcjaAktywujaca where
    show = opis

identycznoscFA :: FunkcjaAktywujaca
identycznoscFA = FunkcjaAktywujaca
    {
      asF = id,
      asF' = const 1,
      opis = "identycznosc"
    }

tanh' x = 1 - (tanh x)^2

tanhFA :: FunkcjaAktywujaca
tanhFA = FunkcjaAktywujaca
    {
      asF = tanh,
      asF' = tanh',
      opis = "tanh"
    }
