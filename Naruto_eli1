module LibraryEli where
import Data.List (intersect)
import PdePreludat

-- --------------------------- Dominio -----------------------
data Ninja = UnNinja {
    nombre :: Nombre,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Rango
} deriving (Eq, Show)

data Herramienta = UnaHerramienta {
    nombreHerramienta :: Nombre, 
    cantDisponible :: Number
} deriving (Eq, Show)

data Mision = UnaMision {
    cantNinjas :: Number,
    rangoRecomendado :: Rango, 
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving (Eq, Show)

-- --------------------------- Definición de Tipos -----------------------
type Nombre = String
type Jutsu = Mision -> Mision
type Rango = Number
type Equipo = [Ninja]
-- --------------------------- Funciones -----------------------
-- ------------ Parte A
-- Funcion A
{-
OTRA IMPLEMENTACION:

obtenerHerramienta :: Nombre -> Number -> Ninja -> Ninja
obtenerHerramienta herramienta cant ninja = obtener herramienta (cantFinal cant ninja) ninja

cantFinal :: Number -> Ninja -> Number
cantFinal cant ninja = min cant (cantPosible ninja)

cantPosible :: Ninja -> Number
cantPosible = (100 -) . sum . map cantDisponible. herramientas 



-}

obtenerHerramienta :: Nombre -> Number -> Ninja -> Ninja
obtenerHerramienta herramienta cant ninja 
    | puedeObtener cant ninja = obtener herramienta cant ninja
    | otherwise = obtener herramienta (cantPosible ninja) ninja

puedeObtener :: Number -> Ninja -> Bool
puedeObtener cant = 
    (<= 100) . (+ cant) . sumaHerramientas

sumaHerramientas :: Ninja -> Number
sumaHerramientas = 
    sum . map cantDisponible. herramientas

obtener :: Nombre -> Number -> Ninja -> Ninja
obtener herramienta cant ninja = agregarHerramientas ninja (UnaHerramienta herramienta cant)

agregarHerramientas :: Ninja -> Herramienta -> Ninja
agregarHerramientas ninja herramienta = ninja {
    herramientas = herramienta : herramientas ninja
    } 

cantPosible :: Ninja -> Number
cantPosible = (100 -) . sumaHerramientas 

-- Funcion B
usarHerramienta :: Ninja -> Herramienta -> Ninja
usarHerramienta ninja herramienta = ninja {herramientas = herramientasSin ninja herramienta}

herramientasSin :: Ninja -> Herramienta -> [Herramienta]
herramientasSin ninja herramienta = filter (herramienta /=) (herramientas ninja) 

-- ------------ Parte B
-- Funcion A
esDesafiante :: Equipo -> Mision -> Bool
esDesafiante ninjas mision = algunoRangoMenor ninjas mision && muchosEnemigos mision

algunoRangoMenor :: Equipo -> Mision -> Bool
algunoRangoMenor ninjas mision = any (rangoEs (<) (rangoRecomendado mision)) ninjas

rangoEs :: (Number -> Number -> Bool) -> Number -> Ninja -> Bool
rangoEs op delta = (`op` delta) . rango

muchosEnemigos :: Mision -> Bool
muchosEnemigos = (>=2) . length . enemigos

-- Funcion B
esCopada :: Mision -> Bool
esCopada mision =
    recompensaEs 3 "Bomba de Humo" mision ||
    recompensaEs 5 "Shuriken" mision || 
    recompensaEs 14 "Kunai" mision

recompensaEs :: Number -> Nombre -> Mision -> Bool
recompensaEs cant nombre = (== UnaHerramienta nombre cant) . recompensa

-- Funcion C
esFactible :: Equipo -> Mision -> Bool
esFactible ninjas mision = (not . esDesafiante ninjas) mision && segundCond ninjas mision

segundCond :: Equipo -> Mision -> Bool
segundCond ninjas mision = 
    cantNecesaria ninjas mision || herramientasEquipoMayor ninjas

herramientasEquipoMayor :: Equipo -> Bool
herramientasEquipoMayor = (>500) . sum . map sumaHerramientas

cantNecesaria :: Equipo -> Mision -> Bool
cantNecesaria ninjas mision =  length ninjas >= cantNinjas mision

-- Funcion D
fallarMision :: Equipo -> Mision -> Equipo
fallarMision equipo mision = map (cambiarRango (-2)) (ninjasRestantes equipo mision)

ninjasRestantes :: Equipo -> Mision -> Equipo
ninjasRestantes equipo mision = filter (rangoEs (>=) (rangoRecomendado mision)) equipo

cambiarRango :: Rango -> Ninja -> Ninja
cambiarRango delta ninja = ninja{rango = rangoMin (rango ninja + delta)}

rangoMin :: Rango -> Rango
rangoMin = max 0 

-- Funcion E
cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision mision = map (cambiosPorCumplir mision) 

cambiosPorCumplir :: Mision -> Ninja -> Ninja
cambiosPorCumplir mision = cambiarRango 1 . obtenerHerramienta (nombreRecompensa mision) (cantRecompensa mision)

nombreRecompensa :: Mision -> Nombre
nombreRecompensa = nombreHerramienta . recompensa

cantRecompensa :: Mision -> Number
cantRecompensa = cantDisponible . recompensa

-- Funcion F
clonesDeSombra :: Number -> Jutsu
clonesDeSombra cant mision = mision{cantNinjas = nuevaCantNinjas cant mision}

nuevaCantNinjas :: Number -> Mision -> Number
nuevaCantNinjas cant mision = max 1 (cantNinjas mision - cant)

-- Funcion G
fuerzaDeUnCentenar :: Jutsu 
fuerzaDeUnCentenar mision = mision {enemigos = enemigosRestantes mision}

enemigosRestantes :: Mision -> [Ninja]
enemigosRestantes mision = filter (rangoEs (<) 500) (enemigos mision)

-- Funcion H
ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision equipo mision = resultadoMision equipo (equipoUsaJutsus (jutsusEquipo equipo) mision) 

resultadoMision :: Equipo -> Mision -> Equipo
resultadoMision equipo mision
    | esCopada mision || esFactible equipo mision = cumplirMision mision equipo 
    | otherwise = fallarMision equipo mision

jutsusEquipo :: Equipo -> [[Jutsu]]
jutsusEquipo = map jutsus

equipoUsaJutsus :: [[Jutsu]] -> Jutsu
equipoUsaJutsus listaJutsus mision = foldl (flip ninjaUsaJutsus) mision listaJutsus

ninjaUsaJutsus :: [Jutsu] -> Jutsu
ninjaUsaJutsus jutsus mision = foldl ejecutarJutsu mision jutsus

ejecutarJutsu :: Mision -> Jutsu -> Mision
ejecutarJutsu mision jutsu = jutsu mision

-- ------------ Parte C
abanicoMadaraUchiha :: Herramienta 
abanicoMadaraUchiha = UnaHerramienta "Abanico Madara Uchiha" 1

zetsu :: Ninja
zetsu = UnNinja "Zetsu " [] [] 600

granGuerraNinja = UnaMision 100000 100 (infinitosEnemigos zetsu 1) abanicoMadaraUchiha

infinitosEnemigos :: Ninja -> Number -> [Ninja]
infinitosEnemigos ninja n = agregarSufijo (show n) ninja : infinitosEnemigos zetsu (n+1)

agregarSufijo :: String -> Ninja -> Ninja
agregarSufijo sufijo ninja = ninja {nombre = nombre ninja ++ sufijo}
