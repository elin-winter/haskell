module Library where
import PdePreludat

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro -> Tiro 

-- Funcion 1 - Modelado de Palos

putter :: Palo
putter habilidad tiro = tiro {velocidad = 10, precision = 2* (precisionJugador habilidad), altura = 0}

madera :: Palo
madera tiro = tiro {velocidad = 100, precision = 0.5*(precisionJugador habilidad), altura = 5}


hierro :: Number -> Palo 
hierro habilidad n tiro = tiro {velocidad = n*(fuerzaJugador habilidad), precision = precisionJugador/n, altura = max 0 (n-3)}

obtenerHierro :: Number -> Palo 
obtenerHierro habilidad n tiro = map hierro [1..10]

type Palos = [Palo]

-- Funcion 2

golpe :: Jugador -> Palo -> Tiro ->Tiro
golpe jugador palo tiro = palo (habilidad jugador) tiro

-- Funcion 3

type Obstaculo = Tiro -> Tiro


estaEntre :: Number  -> Number -> Number -> Number
estaEntre x y z = x > y && x < z

tunel :: Obstaculo
tunel tiro
    |precision > 90 && altura == 0 = tiro {velocidad = 2 * (velocidad tiro), precision = 100}
    |otherwise = tiro {veocidad = 0, precision = 0, altura = 0}

laguna :: Number -> Obstaculo
laguna largo tiro 
    |(velocidad tiro) > 80 && estaEntre (altura tiro) 1 5 = tiro{altura = altura tiro / largo}
    |otherwise = tiro {velocidad = 0, precision = 0, altura = 0}


hoyo :: Obstaculo
hoyo tiro
    |altura == 0 && estaEntre (velocidad tiro) 5 20 & (precision tiro) > 95 = ponerACero tiro
    |otherwise = tiro {velocidad = 0, precision = 0, altura = 0}


ponerACero :: Obstaculo
ponerACero tiro = tiro {velocidad = 0, precision = 0, altura = 0}

-- Funcion 4

esUtil :: Jugador -> Palos -> Obstaculo -> Bool
esUtil jugador palos obstaculo = obstaculo (palo (habilidad jugador)) /= ponerACero (palo (habilidad jugador))

palosUtiles :: Jugador -> Palos -> Obstaculo -> [Palos]
palosUtiles jugador palos obstaculo = filter (esUtil jugador obstaculo) palos

-- Funcion 5 

superarObstaculo :: Tiro -> Obstaculo -> Bool
superarObstaculo tiro obstaculo = obstaculo tiro /= ponerACero 

obstaculosSuperados :: Tiro -> [Obstaculo] -> [Bool]
obstaculosSuperados jugador tiro obstaculos = takeWhile (superarObstaculo tiro) obstaculos

cantObstaculosSuperados :: Tiro -> [Obstaculo] -> Number
cantObstaculosSuperados tiro = length . obstaculosSuperados tiro

-- Funcion 6

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (cantObstaculosSuperados (habilidad jugador) obstaculos) palos
