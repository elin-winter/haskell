module Library where
import PdePreludat

-- ----------------------- Dominio --------------------------

data Jugador = UnJugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
    } deriving (Eq, Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Number,
    precisionJugador :: Number
    } deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

-- ----------------------- Definición de Tipos --------------------------
type Puntos = Number
type Palo = Habilidad -> Tiro
type Obstaculo = Tiro -> Tiro
type Condicion = Tiro -> Bool

palos :: [Palo]
palos = [putter, madera, hierro 3] -- ERROR GRAVISIMO

-- ----------------------- Funciones Útiles --------------------------
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- ----------------------- Ejemplo --------------------------

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

-- ----------------------- Funciones --------------------------
-- -------------- Parte 1

putter :: Palo
putter habilidad = UnTiro { 
    velocidad = 10, 
    precision = 2* precisionJugador habilidad,
    altura = 0
    }

madera :: Palo
madera habilidad = UnTiro { 
    velocidad = 100, 
    precision = precisionJugador habilidad /2,
    altura = 5
    }

hierro :: Number -> Palo
hierro n habilidad 
    | between 1 10 n = UnTiro { 
        velocidad = n* fuerzaJugador habilidad, 
        precision = precisionJugador habilidad /n,
        altura = max 0 (n-3)
    }
    | otherwise = UnTiro {
        velocidad = 0,
        precision = 0,
        altura = 0
    }

-- -------------- Parte 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador

-- -------------- Parte 3
-- Funcion 1
tunel :: Obstaculo
tunel tiro 
    | superaTunel tiro = tiro {
        velocidad = 2* velocidad tiro,
        precision = 100,
        altura = 0
    }
    | otherwise = tiroNulo tiro

superaTunel :: Condicion
superaTunel tiro = precision tiro > 90 && altura tiro == 0

-- Funcion 2
laguna :: Number -> Obstaculo
laguna largo tiro
    | superaLaguna tiro = tiro{
        altura = altura tiro / largo
    }
    | otherwise = tiroNulo tiro

superaLaguna :: Condicion
superaLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

-- Funcion 3
hoyo :: Obstaculo
hoyo tiro 
    | superaHoyo tiro = tiroNulo tiro
    | otherwise = tiroNulo tiro

superaHoyo :: Condicion
superaHoyo tiro = between 5 20 (velocidad tiro) && (altura tiro) == 0 && precision tiro > 95

tiroNulo :: Tiro -> Tiro
tiroNulo tiro = tiro{ velocidad = 0, precision = 0, altura = 0}

-- -------------- Parte 4
-- Funcion 1
palosUtiles :: Jugador -> Obstaculo -> Palo
palosUtiles jugador obstaculo = filter (not. esTiroNulo . obstaculo . (golpe jugador)) palos 

esTiroNulo :: Tiro -> Bool
esTiroNulo tiro = velocidad tiro == 0 && precision tiro == 0 && altura tiro == 0

-- Funcion 2
obstaculosSuperados :: [Obstaculo] -> Tiro -> [Obstaculo]
obstaculosSuperados [] _ = []
obstaculosSuperados (obstaculo:obstaculos) tiro 
    | (not . esTiroNulo . obstaculo) tiro = obstaculo : obstaculosSuperados obstaculos (obstaculo tiro)
    | otherwise = []

-- Funcion 3
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = find ((maxObsSuperados jugador obstaculos).(==).(golpe jugador)) palos

maxObsSuperados :: Jugador -> [Obstaculo] -> Number
maxObsSuperados jugador obstaculos = foldl (max . length) 0 (obsXPalo obstaculos jugador)

obsXPalo :: [Obstaculo] -> Jugador -> [Palo]
obsXPalo obstaculosnjugador = (obstaculosSuperados obstaculos) . (golpe jugador) palos

-- -------------- Parte 5
perdieronApuesta :: [(Jugador, Puntos)] -> [String]
perdieronApuesta puntajes = padresPerdedores puntajes (puntajesPerdedores puntajes)

padresPerdedores :: [(Jugador, Puntos)] -> [Puntos] -> [String]
padresPerdedores  _ [x] = padre . fst (find ((puntajeEs puntos) . snd) puntajes) 
padresPerdedores puntajes (punto:puntos) = padre . fst (find ((puntajeEs puntos) . snd) puntajes) : padresPerdedores puntajes puntos

puntajesPerdedores :: [(Jugador, Puntos)] -> [Puntos]
puntajesPerdedores puntajes = take ((-1) . length puntajes) . sort. (map snd) puntajes

puntajeEs :: Puntos -> Puntos -> Bool
puntajeEs puntos puntaje = puntos == puntaje
