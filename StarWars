module Library where
import PdePreludat

-- --------------------- Dominio --------------------------

data Nave = UnaNave {
    durabilidad :: Durabilidad,
    ataque :: Ataque,
    escudo :: Escudo, 
    poderEspecial :: PoderEspecial
} deriving (Eq, Show)

-- --------------------- Definición de Tipos --------------------------
type Durabilidad = Number
type Ataque = Number
type Escudo = Number
type PoderEspecial = Nave -> Nave
type Flota = [Nave]
type Estrategia = Nave -> Bool

-- --------------------- Modelaje (Ejemplos) --------------------------
tieFighter :: Nave
tieFighter = UnaNave 200 50 100 turbo

xWing :: Nave
xWing = UnaNave 300 100 150 reparacionEmergencia

naveDarthVader :: Nave
naveDarthVader = UnaNave 500 200 300 superTurbo

millenniumFalcon:: Nave
millenniumFalcon = UnaNave 1000 50 500 poderFalcon 

mariposaDestructiva :: Nave
mariposaDestructiva = UnaNave 400 50 20 autodestruccion

-- --------------------- Funciones --------------------------
-- ------------- Parte 1
-- Funcion 1
turbo :: PoderEspecial
turbo = cambiarAtaque 25

cambiarAtaque :: Ataque -> Nave -> Nave
cambiarAtaque delta nave = nave {ataque = ataque nave + delta}

cambiarDurabilidad :: Durabilidad -> Nave -> Nave
cambiarDurabilidad delta nave = nave {durabilidad = durabilidad nave + delta}

cambiarEscudo :: Escudo -> Nave -> Nave
cambiarEscudo delta nave = nave {escudo = escudo nave + delta}

-- Funcion 2
reparacionEmergencia :: PoderEspecial
reparacionEmergencia = cambiarDurabilidad 50 . cambiarAtaque (-30)

-- Funcion 3
superTurbo :: PoderEspecial
superTurbo = cambiarDurabilidad (-45) . turbo . turbo . turbo

-- Funcion 4
poderFalcon :: PoderEspecial
poderFalcon = cambiarEscudo 100 . reparacionEmergencia

-- Funcion 5
autodestruccion :: PoderEspecial
autodestruccion nave = nave {
    durabilidad = 0, 
    escudo = 0, 
    ataque = ataque nave + 10000000000
    }

-- ---------------- Parte 2
-- Funcion 1
durabilidadTotal :: Flota -> Durabilidad
durabilidadTotal = sum . map durabilidad

-- Funcion 2
naveDespuesAtaque :: Nave -> Nave -> Nave
naveDespuesAtaque atacadora victima = reducirDurabilidad (activarPoder atacadora) (activarPoder victima)

reducirDurabilidad :: Nave -> Nave -> Nave
reducirDurabilidad atacadora victima
    | escudo victima > ataque atacadora = victima
    | otherwise = dannioRecibido atacadora victima

dannioRecibido :: Nave -> Nave -> Nave
dannioRecibido atacadora victima = cambiarDurabilidad (calcularDannio atacadora victima) victima

calcularDannio :: Nave -> Nave -> Number
calcularDannio atacadora victima = max 0 (ataque atacadora - escudo victima)

activarPoder :: PoderEspecial
activarPoder nave = poderEspecial nave nave

-- Funcion 3
fueraDeCombate :: Nave -> Bool
fueraDeCombate = (== 0) . durabilidad 

-- Funcion 4
{-

misionSorpresa :: Nave -> Flota -> Estrategia -> Flota
misionSorpresa _ [] _ = []
misionSorpresa nave (x : xs) estrategia
    | estrategia x = ataqueNave x nave : misionSorpresa nave xs estrategia
    |otherwise = x : misionSorpresa nave xs estrategia

-}



misionSorpresa :: Flota -> Nave -> Estrategia -> Flota
misionSorpresa flota nave estrategia = map (naveDespuesAtaque nave) (navesParaAtacar flota estrategia) 

navesParaAtacar :: Flota -> Estrategia -> Flota
navesParaAtacar flota estrategia = filter estrategia flota 

navesDebiles :: Estrategia
navesDebiles = (<200) . escudo

navesPeligrosas :: Ataque -> Estrategia
navesPeligrosas delta = (>delta) . ataque

navesFueraCombate :: Nave -> Estrategia
navesFueraCombate atacadora = fueraDeCombate . naveDespuesAtaque atacadora

navesConAtaqueMayor :: Nave -> Estrategia
navesConAtaqueMayor atacadora victima = ataque atacadora > ataque victima

-- ---------------- Parte 3
determinarEstrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
determinarEstrategia nave flota e1 e2 = misionSorpresa flota nave (mejorEstrategia e1 e2 flota nave)

mejorEstrategia :: Estrategia -> Estrategia -> Flota -> Nave -> Estrategia
mejorEstrategia e1 e2 flota atacante 
    | minimizaDurabilidad e1 e2 atacante flota = e1
    | otherwise = e2

minimizaDurabilidad :: Estrategia -> Estrategia -> Nave -> Flota -> Bool
minimizaDurabilidad e1 e2 atacante flota = durabilidadDespuesMision flota atacante e1 > durabilidadDespuesMision flota atacante e2 

durabilidadDespuesMision :: Flota -> Nave -> Estrategia -> Durabilidad
durabilidadDespuesMision flota nave e = durabilidadTotal (misionSorpresa flota nave e) 

-- ---------------- Parte 4
flotaInfinita :: Nave -> Flota
flotaInfinita nave = nave : flotaInfinita nave

{-
¿Es posible determinar su durabilidad total? 

Sería imposible, incluso con el motor de lazy evaluation de Haskell, en este caso en particular
es necesario evaluar la durabilidad de cada uno de los elementos de la lista para poder
sumarlos al final, si se trata de una lista infinita, nunca va a poder terminar de calcular la 
durabilidad de cada uno. 

¿Qué se obtiene como respuesta cuando se lleva adelante una
misión sobre ella? 

Depende de la estrategia elegida. Para dar un ejemplo, si la estrategia fuera atacar solo a
la primer nave de la flota, entonces se filtraría solo esta y se podría aplicar la misión.
Como requiere un filtro previo, hay casos en donde se podría llevar adelante, otros no. 
Si una strategia fuera elegir todas las naves que tuvieran de ataque 50, nunca terminaría 
de evaluar la condición y no podría realizar la misión. 

Se debe al hecho de que Haskell utiliza evaluación diferida (lazy evaluation), por lo que primero tiene
en cuenta la función a realizar y despues los parametros que utilizará. 

En nuestro caso particular, todas las estrategias que modelamos no podrían llevar adelante
la misión ante una flota infinita. 

-}
