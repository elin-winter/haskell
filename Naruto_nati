module LibraryNati where
import PdePreludat

type Nombre = String
type Rango = Number
type Jutsus = Mision -> Mision

data Ninja = UnNinja {
    nombre :: Nombre,
    rango :: Rango,
    herramientas :: [Herramienta],
    jutsus :: [Jutsus]
}deriving (Show, Eq)

data Herramienta = UnaHerramienta {
    nombreHerramienta :: Nombre,
    cantDisponible :: Number
}deriving (Show, Eq)

rangoMin :: Rango -> Rango
rangoMin = max 0 

bombasDeHumo :: Herramienta
bombasDeHumo = UnaHerramienta "Bombas de Humo" 3

shurikens :: Herramienta
shurikens = UnaHerramienta "Shurikens" 10

kunais :: Herramienta
kunais = UnaHerramienta "Kunais" 2

--Parte A

--Ejercicio a

obtenerHerramienta :: Number -> Ninja -> Herramienta -> Ninja
obtenerHerramienta cant ninja herramienta
    |puedeObtenerHerramienta cant ninja = agregarNHerramientas cant ninja herramienta
    |otherwise = agregarNHerramientas (maximoHerramientas ninja) ninja herramienta

maximoHerramientas ::  Ninja -> Number
maximoHerramientas ninja =  100 - sumHerramientas ninja  

agregarNHerramientas ::  Number -> Ninja -> Herramienta -> Ninja
agregarNHerramientas cant ninja herramienta = ninja {herramientas = herramientas ninja ++ obtenerNHerramientas cant herramienta}

-- Herramientas tiene una cant
obtenerNHerramientas ::  Number -> Herramienta -> [Herramienta]
obtenerNHerramientas  = replicate  

puedeObtenerHerramienta :: Number -> Ninja -> Bool
puedeObtenerHerramienta cant  = (<= 100) . (+ cant) . sumHerramientas

sumHerramientas :: Ninja -> Number
sumHerramientas = length . herramientas

--Ejercicio b

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta = perderHerramienta 

listaSinHerramienta :: Herramienta -> [Herramienta] -> [Herramienta]
listaSinHerramienta _ [] = []
listaSinHerramienta herramientaBuscada (h : hs)
    |herramientaBuscada == h = hs
    |otherwise = h : listaSinHerramienta herramientaBuscada hs


perderHerramienta :: Herramienta -> Ninja -> Ninja
perderHerramienta herramienta ninja = ninja {herramientas = listaSinHerramienta herramienta (herramientas ninja)}

--Parte B


data Mision = UnaMision {
    cantNinjas :: Number,
    rangoRecomendado :: Rango,
    ninjasEnemigos :: [Ninja],
    herramientaRecompensa :: [Herramienta]
}


-- Funcion a
esDesafiante :: Mision -> [Ninja] -> Bool
esDesafiante mision equipo = tieneMenorRangoRecomendado mision equipo && derrotarMinDosEnemigos (ninjasEnemigos mision)

tieneMenorRangoRecomendado :: Mision -> [Ninja] -> Bool
tieneMenorRangoRecomendado mision  = any (menorRangoRecomendado (rangoRecomendado mision))

menorRangoRecomendado :: Rango -> Ninja -> Bool
menorRangoRecomendado rangoRecomendado = (< rangoRecomendado) . rango

derrotarMinDosEnemigos :: [Ninja] -> Bool
derrotarMinDosEnemigos = (>=2) . length 

-- Funcion b
esCopada :: Mision -> Bool
esCopada = cumpleCondEsCopada 
    
cumpleCondEsCopada :: Mision -> Bool
cumpleCondEsCopada mision = 
    tieneCantHerramientas 3 bombasDeHumo (herramientaRecompensa mision) || 
    tieneCantHerramientas 5 shurikens (herramientaRecompensa mision) || 
    tieneCantHerramientas 14 kunais (herramientaRecompensa mision)

tieneCantHerramientas :: Number -> Herramienta -> [Herramienta] -> Bool
tieneCantHerramientas cant herramienta = (>=  cant) . cantHerramientas herramienta 

cantHerramientas :: Herramienta -> [Herramienta] -> Number
cantHerramientas herramienta = length . filtrarHerramientas herramienta

filtrarHerramientas :: Herramienta -> [Herramienta] -> [Herramienta]
filtrarHerramientas herramientaBusc = filter (== herramientaBusc) 


--Funcion c

esFactible ::  Mision -> [Ninja] -> Bool
esFactible mision equipo = not (esDesafiante mision equipo) && segCondEsFactible mision equipo

segCondEsFactible :: Mision -> [Ninja] -> Bool
segCondEsFactible mision equipo = cantNinjasNecesaria mision equipo || sumaHerramientasMayorA 500 equipo

cantNinjasNecesaria ::  Mision -> [Ninja] -> Bool
cantNinjasNecesaria mision = (>= cantNinjas mision) . length

cantHerramientasEquipo :: [Ninja] -> [Number]
cantHerramientasEquipo = map sumHerramientas 

sumaHerramientasMayorA :: Number -> [Ninja] -> Bool
sumaHerramientasMayorA valor equipo = sum (cantHerramientasEquipo equipo) > valor

-- Funcion d

fallaMision :: Mision -> [Ninja] -> [Ninja]
fallaMision mision = modificarRangoEquipo (-2) . equipoDisminuido mision

modificarRangoEquipo :: Rango ->  [Ninja] -> [Ninja]
modificarRangoEquipo rango = map (modificarRango rango) 

modificarRango :: Rango -> Ninja -> Ninja
modificarRango rangoNuevo ninja = ninja {rango = rangoMin (rango ninja + rangoNuevo)}

equipoDisminuido :: Mision -> [Ninja] -> [Ninja]
equipoDisminuido mision = filter (mayorRangoRecomendado (rangoRecomendado mision))

mayorRangoRecomendado :: Rango -> Ninja -> Bool
mayorRangoRecomendado rangoRecomendado = (>= rangoRecomendado) . rango

-- Funcion e

cumplirMision :: Mision -> [Ninja] -> [Ninja]
cumplirMision mision = equipoObtieneRecompensa mision . modificarRangoEquipo 1

equipoObtieneRecompensa :: Mision -> [Ninja] -> [Ninja]
equipoObtieneRecompensa mision = map (agregarRecompensa (herramientaRecompensa mision))

agregarRecompensa :: [Herramienta]  ->  Ninja  ->  Ninja
agregarRecompensa recompensa ninja = foldl (obtenerHerramienta 1) ninja recompensa


--Funcion f


clonesDeSombra :: Number -> Jutsus
clonesDeSombra = modificarCantNinjas 

modificarCantNinjas :: Number -> Jutsus
modificarCantNinjas cantClones mision  = mision {cantNinjas = max 1 (cantNinjas mision - cantClones)}

-- Funcion g

fuerzaDeUnCentenar :: Number ->Jutsus
fuerzaDeUnCentenar  = modificarMision 


modificarMision :: Number -> Jutsus
modificarMision cantClones mision = mision { ninjasEnemigos = filtrarEnemigos mision }

filtrarEnemigos ::  Mision -> [Ninja]
filtrarEnemigos mision = filter (rangoMayorA 500) (ninjasEnemigos mision)

rangoMayorA :: Number -> Ninja -> Bool
rangoMayorA valor  = (> valor) . rango


-- Funcion h

ejecutarMision ::  [Ninja] ->Mision->Mision
ejecutarMision equipo mision
    |seCumpleMision (usarJutsus equipo mision) equipo = cumplirMision mision equipo
    |otherwise = fallaMision mision equipo 

aplicarJutsu :: Jutsus -> Mision -> Mision
aplicarJutsu jutsu  = jutsu 

usarJutsus :: [Ninja] -> Mision -> Mision
usarJutsus [] mision = mision 
usarJutsus (n:ns) mision = usarJutsus ns (aplicarJutsu (jutsus ninja) mision)


seCumpleMision :: Mision -> [Ninja] ->  Bool
seCumpleMision mision equipo = esCopada mision &&  esFactible mision equipoDisminuido

-- Parte C

granGuerraNinja :: Mision
granGuerraNinja = UnaMision 100000 100 infinitosEnemigos 1 

modificarVillano:: Number  -> Ninja -> Ninja
modificarVillano n ninja = ninja {nombre = "Zetsu" ++ [show n], rango = 600}


infinitosEnemigos ::  String  -> [Ninja] -> [Ninja]
infinitosEnemigos n (e : es) = modificarVillano n e : infinitosEnemigos (n+1) es

{-
a) Considerando que Haskell opera con Lazy Evaluation, esta funcion siempre devolverá un resultado, ya que al verificar que hay al menos dos enemigos, se deja de recorrer la funcion. 

b) En este caso siempre va a devolver un resultado ya que la condicion esta sobre la mision, no el equipo de Ninjas. Por ende, no afecta que este sea infinito.

c) 
-}
