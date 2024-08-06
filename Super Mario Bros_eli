module LibraryEli where
import Data.Char (isUpper)
import PdePreludat

-- ---------------------- Dominio -------------------------

data Plomero = UnPlomero {
    nombre :: Nombre, 
    cajaHeramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Dinero
}

data Herramienta = UnaHerramienta {
    denominacion :: Nombre, 
    precio :: Dinero,
    materialEmpunniadura :: Material
}

data Reparacion = UnaReparacion {
    descripcion :: String, 
    requerimiento :: Requerimiento
}

data Material = Hierro | Madera | Goma | Plastico

-- ---------------------- Definición de Tipos -----------------
type Nombre = String
type Dinero = Number
type Requerimiento = Plomero -> Bool
type JornadaDeTrabajo = [Reparacion]
type Criterio = JornadaDeTrabajo -> [Plomero] -> Plomero

-- ---------------------- Ejemplos --------------------------------
-- --------------- Plomeros
mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa, martillo] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" [herramientasInfinitas 1 llaveFrancesa] [] 0.5

-- --------------- Herramientas
martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 Madera

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave Inglesa" 200 Hierro

llaveFrancesa :: Herramienta
llaveFrancesa = UnaHerramienta "Llave Francesa" 1 Hierro

destornillador :: Herramienta
destornillador = UnaHerramienta "Destornillador" 0 Plastico

-- --------------- Reparaciones
filtracionAgua :: Reparacion
filtracionAgua = UnaReparacion "Filtracion de Agua" (herramientaEspecifica "Llave Inglesa")


-- ---------------------- Funciones -----------------------------
-- Funcion 1
herramientasInfinitas :: Number -> Herramienta -> [Herramienta]
herramientasInfinitas delta herramienta = 
    herramienta : herramientasInfinitas (delta + 1) $ precioModificado (delta + 1) herramienta

precioModificado :: Number -> Herramienta -> Herramienta
precioModificado delta herramienta = herramienta{precio = precio herramienta + delta}

-- Funcion 2
esMalvado :: Plomero -> Bool
esMalvado plomero = "Wa" == take 2 (nombre plomero)

-- Funcion 3
puedeComprarHerramienta :: Plomero -> Herramienta -> Bool
puedeComprarHerramienta plomero herramienta = dinero plomero >= precio herramienta

-- Funcion 4
esBuena :: Herramienta -> Bool
esBuena herramienta = primeraCond herramienta || segundaCond herramienta

primeraCond :: Herramienta -> Bool
primeraCond herramienta = 
    materialEmpunniadura herramienta == Hierro && precio herramienta > 10000

segundaCond :: Herramienta -> Bool
segundaCond herramienta = denominacion herramienta == "Martillo" && (mangoEs Madera herramienta || mangoEs Goma herramienta)

mangoEs :: Material -> Herramienta -> Bool
mangoEs material herramienta = materialEmpunniadura herramienta == material

-- FALTA MARTILLOOOOOO

-- Funcion 5
comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta plomero herramienta
    | puedeComprarHerramienta plomero herramienta = comprar plomero herramienta
    | otherwise = plomero

comprar :: Plomero -> Herramienta -> Plomero
comprar herramienta = 
    agregarAPlomero herramienta cajaHeramientas . modificarDinero (negate . precio herramienta)

agregarAPlomero :: Plomero -> a -> (Plomero -> [a]) -> Plomero
agregarAPlomero plomero elemento prop = 
    plomero { prop = elemento : prop plomero}

modificarDinero :: Dinero -> Plomero -> Plomero
modificarDinero plata plomero = plomero { dinero = dinero plomero + plata}

-- Funcion 6 /// FUNCION 1 ???
herramientaEspecifica :: Nombre -> Requerimiento
herramientaEspecifica herramienta plomero = elem herramienta (nombresHerramientas plomero)

nombresHerramientas :: Plomero -> [Nombre]
nombresHerramientas plomero = map denominacion (cajaHeramientas plomero)

-- Funcion 7
esDificil :: Reparacion -> Bool
esDificil reparacion = muchosCaracteres reparacion && esGrito reparacion

muchosCaracteres :: Reparacion -> Bool
muchosCaracteres = (>100). length . descripcion

esGrito :: Reparacion -> Bool
esGrito = all isUpper . descripcion 

-- Funcion 8
presupuestoReparacion :: Reparacion -> Dinero
presupuestoReparacion = (3*) . length . descripcion

-- Funcion 9
hacerReparacion :: Plomero -> Reparacion -> Plomero
hacerReparacion plomero reparacion
    | puedeHacerReparacion plomero reparacion = reparar plomero reparacion
    | otherwise = modificarDinero 100 plomero

puedeHacerReparacion :: Plomero -> Reparacion -> Bool
puedeHacerReparacion plomero reparacion = cumpleRequerimiento plomero reparacion || esMalvadoConMartillo plomero

cumpleRequerimiento :: Plomero -> Reparacion -> Bool
cumpleRequerimiento plomero reparacion = requerimiento reparacion plomero

esMalvadoConMartillo :: Plomero -> Bool
esMalvadoConMartillo plomero = esMalvado plomero && herramientaEspecifica "Martillo"

reparar :: Plomero -> Reparacion -> Plomero
reparar reparacion = 
    modificarDinero (presupuestoReparacion reparacion) . agregarAPlomero historialReparaciones reparacion . comoEs reparacion

comoEs :: Reparacion -> Plomero -> Plomero
comoEs reparacion plomero
    | esMalvado plomero = robarCliente plomero
    | esDificil reparacion = perderHerramientasBuenas plomero
    | otherwise = olvidarHerramienta plomero

robarCliente :: Plomero -> Plomero
robarCliente plomero = agregarAPlomero plomero destornillador cajaHeramientas 

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas plomero = plomero {cajaHeramientas = sinHerramientasBuenas (cajaHeramientas plomero)}

sinHerramientasBuenas :: [Herramienta] -> [Herramienta]
sinHerramientasBuenas = filter (not . esBuena) 

olvidarHerramienta :: Plomero -> Plomero
olvidarHerramienta plomero = plomero { cajaHeramientas = (tail . cajaHeramientas) plomero}

-- Funcion 10
afectarPlomero :: JornadaDeTrabajo -> Plomero -> Plomero
afectarPlomero reparaciones plomero = foldl hacerReparacion plomero reparaciones

-- Funcion 11
masReparador :: Criterio
masReparador = compararPlomeros historialReparaciones 

compararPlomeros :: (Plomero -> Number) -> Criterio
compararPlomeros reparaciones plomeros = elegirUno (plomerosTrabajan reparaciones plomeros)

elegirUno :: [Plomero] -> (Plomero -> Number) -> Plomero
elegirUno [x] _ = x
elegirUno (plomero1:plomero2:plomeros) prop
    | compararProp plomero1 plomero2 prop = elegirUno (plomero1:plomeros) prop
    | compararProp plomero2 plomero1 prop = elegirUno (plomero2:plomeros) prop

compararProp :: Plomero -> Plomero -> (Plomero -> Number) -> Bool
compararProp p1 p2 prop = prop p1 >= prop p2

plomerosTrabajan :: JornadaDeTrabajo -> [Plomero] -> [Plomero]
plomerosTrabajan reparaciones = map (afectaPlomero reparaciones)

-- Funcion 12
masAdinerado :: Criterio
masAdinerado = compararPlomeros dinero 

-- Funcion 12
invirtioMas :: Criterio
invirtioMas = compararPlomeros sumaDineroHerramientas

sumaDineroHerramientas :: Plomero -> Number
sumaDineroHerramientas = (sum . map precio) cajaHeramientas 
