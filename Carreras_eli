module Library_eli where
import PdePreludat

-- --------------------------- Dominio ----------------------------
data Auto = UnAuto{
    color :: Color,
    velocidad :: Velocidad,
    distancia :: Distancia 
}

type Carrera = [Auto]

-- --------------------------- Definición de Tipos ----------------------------
type Color = String
type Velocidad = Number
type Distancia = Number
type Modificador = Number -> Number
type PowerUp = Auto -> Carrera -> Carrera
type Criterio = Auto -> Auto -> Bool
type Evento = Carrera -> Carrera
type TablaPosiciones = [(Number, Color)]

-- --------------------------- Ejemplos ----------------------------

carrera :: Carrera
carrera = [redBull, ferrari, macClaren, fiat]

redBull :: Auto
redBull = UnAuto "Rojo" 120 0

ferrari :: Auto
ferrari = UnAuto "Blanco" 120 0

macClaren :: Auto
macClaren = UnAuto "Azul" 120 0

fiat :: Auto
fiat= UnAuto "Negro" 120 0

eventosCarrera1 :: [Evento]
eventosCarrera1 = 
    [correnTodos 30, 
    jetPack 3 (encontrarAuto azul carrera), 
    terremoto (encontrarAuto blanco carrera), 
    correnTodos 40, 
    miguelitos 20 (encontrarAuto blanco carrera), 
    jetPack 6 (encontrarAuto negro carrera),
    correnTodos 10
    ]

-- --------------------------- Funciones ----------------------------
-- ----------------- Parte 1
-- Función 1 
estaCerca :: Criterio
estaCerca auto1 auto2 = sonDistintos (color auto1) auto2 && dist auto1 auto2 < 10

sonDistintos :: Color -> Auto -> Bool
sonDistintos color auto2 = color /= color auto2

dist :: Auto -> Auto -> Number
dist auto1 auto2 = abs (distancia auto1 - distancia auto2)

-- Función 2
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo = ningunoCerca && vaGanando

ningunoCerca :: Auto -> Carrera -> Bool
ningunoCerca auto carrera = not . any (estaCerca auto) (autosExcepto auto carrera)

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = (not . null) && recorrioMasDistancia auto

recorrioMasDistancia :: Auto -> Carrera -> Bool
recorrioMasDistancia auto carrera = all (leGana auto) (listaDistancias auto carrera)

autosExcepto :: Auto -> Carrera -> [Auto]
autosExcepto auto = filter (sonDistintos auto)

listaDistancias :: Auto -> Carrera -> [Distancia]
listaDistancias auto carrera = map distancia (autosExcepto auto carrera)

-- Función 3
quePuesto :: Auto -> Carrera -> Number
quePuesto = (1+) . leGanan

leGanan :: Auto -> Carrera -> Number
leGanan auto carrera = length . filter (not . leGana auto) (listaDistancias auto carrera)

-- ----------------- Parte 2
-- Funcion 1
autoCorra :: Auto -> Number -> Auto
autoCorra = auto{ distancia = distSegunTiempo}

distSegunTiempo :: Auto -> Number -> Distancia
distSegunTiempo auto tiempo = (distancia auto) + tiempo * (velocidad auto)

-- Funcion 2
modificarVelocidad :: Modificador -> Auto -> Auto
modificarVelocidad modificador auto = auto{ velocidad = modificador (velocidad auto)}

bajarVelocidad :: Number -> Modificador
bajarVelocidad cantidad numeroPrevio = max 0 (numeroPrevio - cantidad) 

-- ----------------- Parte 3

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- Funcion 1
terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (modificarVelocidad (bajarVelocidad 50)) (autosExcepto auto carrera)

-- Funcion 2
miguelitos :: Number -> PowerUp
miguelitos cantidad auto carrera = afectarALosQueCumplen (leGana auto) (modificarVelocidad (bajarVelocidad cantidad)) (autosExcepto auto carrera)

leGana :: Criterio
leGana auto1 auto2 = ((<) . distancia auto) (distancia auto2)

-- Funcion 3
jetPack :: Number -> PowerUp
jetPack tiempo auto = afectarALosQueCumplen auto (modificarVelocidad (/2) . (flip autoCorra tiempo). modificarVelocidad (*2))

-- ----------------- Parte 4

correnTodos :: Number -> Carrera -> Carrera
correnTodos tiempo = map (flip autoCorra tiempo) 

usaPowerUp :: Color -> Carrera -> PowerUp
usaPowerUp color powerup = powerup (encontrarAuto color carrera)

encontrarAuto :: Color -> Carrera -> Auto
encontrarAuto color = find (not . sonDistintos color) 

aplicarEventos :: Carrera -> [Evento] -> Carrera
aplicarEventos carrera eventos = foldl aplicarEvento carrera eventos

aplicarEvento :: Carrera -> Evento -> Carrera
aplicarEvento carrera evento = evento carrera

puestoEs :: Number -> Auto -> Bool
puestoEs num = quePuesto auto == num

crearTablaPosiciones :: Carrera -> Number -> TablaPosiciones
crearTablaPosiciones [] _ = []
crearTablaPosiciones [_] _ = [_]
crearTablaPosiciones (auto:autos) num = (num, color (head $ filter (puestoEs num) (auto:autos))): crearTablaPosiciones autos (n+1)
    
simularCarrera :: Carrera -> [Evento] -> TablaPosiciones
simularCarrera carrera eventos = crearTablaPosiciones (aplicarEventos carrera eventos) 1
