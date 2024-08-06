module Library where
import PdePreludat

type Color = String

data Auto = UnAuto {
    color :: Color,
    velocidad :: Number, 
    distancia :: Number 
} deriving (Show, Eq)


type Carrera = [Auto]

-- Funcion 1

cercaDeAuto :: Auto -> Auto -> Bool
cercaDeAuto auto1 auto2 = (sonDistintos auto1 (color auto2)) && (calculoDistancia auto1 auto2 < 10 )

sonDistintos :: Auto -> Color -> Bool
sonDistintos auto1 color = color auto1 /= color 

calculoDistancia :: Auto -> Auto -> Number
calculoDistancia auto1 auto2 = abs (distancia auto1 - distancia auto2)

-- Funcion 2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto autos = ningunoCercaDeOtrosAutos  && mayorDistanciaRecorrida

-- Agregar que no tiene que considerar al mismo auto
ningunoCercaDeOtrosAutos :: Auto -> Carrera -> Bool
ningunoCercaDeOtrosAutos auto =  not . any (cercaDeAuto auto) 

-- Fijarme que la lista no este vacia
mayorDistanciaRecorrida :: Auto -> Carrera -> Bool
mayorDistanciaRecorrida auto = all (compararDistancia auto)

compararDistancia :: Auto -> Auto -> Bool
compararDistancia auto1 auto2 = distancia auto1 > distancia auto2

--Funcion 3

cantidadAutosAdelante ::  Auto -> Carrera -> Carrera
cantidadAutosAdelante auto autos = filter (not .compararDistancia auto) autos

-- Mejor, ya que si esta primero, la lista es vacia
puestoAuto :: Auto -> Carrera -> Number
puestoAuto auto autos = length (cantidadAutosAdelante auto autos) + 1

{- Anterior
puestoAuto :: Auto -> Carrera -> Number
puestoAuto auto autos
    |mayorDistanciaRecorrida auto autos = 1
    |otherwise = length (cantidadAutosAdelante auto autos) + 1
-}

-- Funcion 4

autoCorre :: Auto -> Number -> Auto
autoCorre auto tiempo = auto {distancia = distanciaRecorrida (distancia auto) tiempo (velocidad auto)} 

distanciaRecorrida :: Number -> Number -> Number -> Number
distanciaRecorrida distancia tiempo velocidad = distancia + tiempo * velocidad

-- Funcion 5

type Modificador = Number -> Number 

modificador :: Number -> Modificador
modificador velocidad x = max 0 (velocidad - x)

-- Considerar Modificador Number->Number
-- Modificador: bajarVel 

modificarVelocidadAuto ::  Auto -> Number -> Auto
modificarVelocidadAuto auto x = auto {velocidad = modificador (velocidad auto) x}

-- Funcion 6

type PowerUp = Auto -> Carrera -> Carrera

-- Agregar 
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp 
terremoto auto = afectarALosQueCumplen (cercaDeAuto auto) (modificarVelocidadAuto 50)

miguelitos :: Number -> PowerUp
miguelitos auto x = afectarALosQueCumplen (compararDistancia auto) (modificarVelocidadAuto x) 


-- Me devuelve una carrera
jetPack :: Number -> PowerUp
jetPack auto tiempo = afectarALosQueCumplen auto (modificarVelocidadAuto (velocidad auto). autoCorre (tiempo) . modificarVelocidadAuto (-(velocidad auto)))


-- Funcion 7

correnTodos :: Carrera -> Number -> Carrera
correnTodos tiempo = map (flip autoCorre tiempo)
{-Falto usar encontrar auto -}

aplicarPowerUp :: (Auto -> Number -> Auto) -> Number -> Color -> Carrera -> Carrera
aplicarPowerUp powerUp color auto = powerUp (encontrarAuto auto color) 

encontrarAuto :: Carrera -> Color -> Auto
encontrarAuto  color = find (not . sonDistintos color)  

-----Carrera



type Evento = Carrera -> Carrera

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

carrera1 :: Evento
carrera1 = 
    [correnTodos 30, 
    jetPack 3 (encontrarAuto azul carrera), 
    terremoto (encontrarAuto blanco carrera), 
    correnTodos 40, 
    miguelitos 20 (encontrarAuto blanco carrera), 
    jetPack 6 (encontrarAuto negro carrera),
    correnTodos 10
    ]

aplicarEvento :: Carrera -> (Carrera -> Carrera) -> Carrera
aplicarEvento carrera evento = evento carrera

aplicarEventos ::Carrera -> [Carrera -> Carrera] -> Carrera
aplicarEventos carrera eventos = foldl aplicarEvento carrera eventos

aplicarPuestoAuto ::  Carrera -> [Number]
aplicarPuestoAuto carrera  =  map puestoAuto carrera

ordenarPosiciones :: Carrera -> [Number]
ordenarPosiciones carrera = sort (aplicarPuestoAuto carrera)

crearTablaPosicionesOrdenada :: Carrera -> [(Number, Color)]
crearTablaPosiciones [] = []
crearTablaPosiciones (lider : autos) = ((head(ordenarPosiciones (lider : autos)), color (head(ordenarPosiciones (lider : autos)))) : crearTablaPosiciones autos)

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Number, Color)]
simularCarrera carrera eventos = crearTablaPosiciones (aplicarEventos carrera eventos)
