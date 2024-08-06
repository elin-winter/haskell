module Library where
import PdePreludat

-- ---------------------- Dominio -------------------------
data Personaje = UnPersonaje {
    nombre :: Nombre, 
    dinero :: Dinero,
    felicidad :: Felicidad
} deriving (Show, Eq)

-- ---------------------- Definicion de Tipos -------------------------
type Nombre = String
type Dinero = Number
type Felicidad = Number

type Actividad = Personaje -> Personaje
type Logro = Personaje -> Bool
-- ---------------------- Ejemplos -------------------------

homero :: Personaje
homero = UnPersonaje "Homero Simpson" 0.5 200

skinner :: Personaje
skinner = UnPersonaje "Skinner" 1000 30

lisa :: Personaje
lisa = UnPersonaje "Lisa Simpson" 20 500

srBurns :: Personaje
srBurns = UnPersonaje "Senior Burns" 10000000000000000000 500

-- ---------------------- Funciones Genericas -------------------------
-- ------------ Personaje
felicidadSegunF :: (Felicidad -> Felicidad) -> Personaje -> Personaje
felicidadSegunF f personaje = personaje {felicidad = max 0 . f . felicidad $ personaje}

dineroSegunF :: (Dinero -> Dinero) -> Personaje -> Personaje
dineroSegunF f personaje = personaje {dinero = f . dinero $ personaje}

nombreSegunF :: (Nombre -> Nombre) -> Personaje -> Personaje
nombreSegunF f personaje = personaje {nombre = f . nombre $ personaje}

-- ---------------------- Funciones -------------------------
-- ------------ Parte 1
-- Funcion 1
irEscuela :: Actividad
irEscuela personaje = 
    felicidadSegunF (cantSegunNombre personaje) personaje

cantSegunNombre :: Personaje -> (Felicidad -> Felicidad)
cantSegunNombre personaje 
    | nombre personaje == "Lisa" = (+20)
    | otherwise = subtract 20    

-- Funcion 2
comerDonas :: Number -> Actividad
comerDonas cant = 
    felicidadSegunF (felicidadXDona cant) . dineroSegunF (subtract 10)

felicidadXDona :: Number -> (Number -> Number)
felicidadXDona cant = (+(cant * 10))

-- Funcion 3
irTrabajar :: String -> Actividad
irTrabajar laburo = dineroSegunF (+ length laburo)

-- Funcion 4
irTrabajarDirector :: Actividad
irTrabajarDirector = irTrabajar "Escuela Elemental" . irEscuela

-- Funcion 5
irALaFacu :: Actividad
irALaFacu = 
    felicidadSegunF (subtract 1000000) . nombreSegunF (agregarPrefijo "Ingeniero ")

agregarPrefijo :: String -> Nombre -> Nombre
agregarPrefijo prefijo nombre = prefijo ++ nombre 

{-

EJEMPLOS DE USO:

- Homero come una docena de donas

> comerDonas 12 homero
UnPersonaje
    { nombre = "Homero Simpson"
    , dinero = -9.5
    , felicidad = 320
    }

- Skinner va a trabajar como director

> irTrabajarDirector skinner
UnPersonaje
    { nombre = "Skinner"
    , dinero = 1017
    , felicidad = 10
    }

- Lisa va a la escuela y luego realiza la actividad inventada.

> (irEscuela . irALaFacu) lisa
UnPersonaje
    { nombre = "Ingeniero Lisa Simpson"
    , dinero = 20
    , felicidad = 0
    }

-}

-- ------------ Parte 2
propMayorA :: (Personaje -> Number) -> Number -> Personaje -> Bool
propMayorA prop cant = (>cant) . prop

-- Funcion 1
esMillonario :: Logro
esMillonario = propMayorA dinero (dinero srBurns)

-- Funcion 2
alegrarse :: Felicidad -> Logro
alegrarse = propMayorA felicidad

-- Funcion 3
verProgramaKrosti :: Logro
verProgramaKrosti = (>= 10) . dinero

-- Funcion 4 (Inventada)
comprarAuto :: Logro
comprarAuto = propMayorA dinero 999999999

-- Funcion 5
esDecisiva :: Actividad -> Logro -> Personaje -> Bool
esDecisiva act logro personaje = (logro . act) personaje && (not . logro) personaje 

-- Funcion 6
intentarAlcanzarLogro :: Personaje -> Logro -> [Actividad] -> Personaje
intentarAlcanzarLogro personaje logro acts = (aplicarAct personaje logro acts) personaje

aplicarAct :: Personaje -> Logro -> [Actividad] -> Actividad
aplicarAct _ _ [] = id
aplicarAct personaje logro (act:acts)
    | esDecisiva act logro personaje = act
    | otherwise = aplicarAct personaje logro acts


-- Funcion 7
infinitasActs :: [Actividad]
infinitasActs = cycle [irALaFacu , irEscuela, irTrabajar "Ferreteria"]

{-
EJEMPLO DE USO:

> intentarAlcanzarLogro lisa comprarAuto infinitasActs 
^CInterrupted.

(No resultó en nada)

> intentarAlcanzarLogro homero verProgramaKrosti infinitasActs
UnPersonaje
    { nombre = "Homero Simpson"
    , dinero = 10.5
    , felicidad = 200
    }

-}
