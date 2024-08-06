module LibraryNati where
import PdePreludat
import Data.List (intersect)
import Data.Char (isUpper)


type Nombre = String
type Dinero = Number
type Problema = String
type  Requerimiento = Plomero -> Bool

data Material = Hierro | Madera | Goma | Plastico

data Plomero = UnPlomero{
    nombre :: Nombre,
    herramienta :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Dinero
}

data Herramienta = UnaHerramienta{
    nombreHerramienta :: Nombre,
    precio :: Dinero,
    material :: Material
}

data Reparacion = UnaReparacion {
    descripcionProblema :: Problema,
    requerimiento :: Requerimiento
}

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa,martillo] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" (llaveFrancesa 1) []  0.50

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave Inglesa" 200 Hierro

martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 Madera

modelarLlaveFrancesa :: Number -> Herramienta
modelarLlaveFrancesa n = UnaHerramienta "Llave Francesa" n Hierro

llaveFrancesa :: Number -> [Herramienta]
llaveFrancesa n = modelarLlaveFrancesa n : llaveFrancesa (n+1)

destornillador :: Herramienta
destornillador = UnaHerramienta "Destornillador" 0 Plastico 

filtracionAgua = UnaReparacion "Filtración de Agua" (tieneHerramientaConCiertaDenominacion "LLave Francesa")

--Parte 1

--Funcion 1

tieneHerramientaConCiertaDenominacion :: Nombre -> Plomero  -> Bool
tieneHerramientaConCiertaDenominacion denominacion plomero = any ((==denominacion) . nombreHerramienta) (herramienta plomero)

--Funcion 2

esMalvado :: Plomero  -> Bool
esMalvado plomero = take 2 (nombre plomero) == "Wa" 

--Funcion 3

puedeComprarHerramienta :: Herramienta -> Plomero-> Bool
puedeComprarHerramienta herramienta plomero = dinero plomero >= precio herramienta

--Funcion 4

esNombreHerramienta :: Nombre ->Herramienta -> Bool
esNombreHerramienta nombre herramienta = nombre herramienta == nombre

esDeCiertoMaterial ::  Material -> Herramienta -> Bool
esDeCiertoMaterial material herramienta = material herramienta == material

precioHerramientaMayorA :: Precio -> Herramienta -> Bool
precioHerramientaMayorA costo herramienta = precio herramienta > costo 

primeraCondEsBuena :: Herramienta -> Bool
primeraCondEsBuena herramienta = esDeCiertoMaterial Hierro herramienta && precioHerramientaMayorA 10000 herramienta

segundaCondEsBuena ::  Herramienta -> Bool
segundaCondEsBuena herramienta = esNombreHerramienta "Martillo" herramienta && (esDeCiertoMaterial Madera herramienta || esDeCiertoMaterial Goma)

herramientaEsBuena :: Herramienta -> Bool
herramientaEsBuena herramienta = primeraCondEsBuena herramienta || segundaCondEsBuena 

--Funcion 5

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero {herramienta = herramienta plomero ++ [herramienta]}

pagarHerramienta :: Herramienta -> Plomero -> Plomero
pagarHerramienta herramienta plomero = plomero {dinero = dinero plomero - precio herramienta}

comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta plomero herramienta 
    |puedeComprarHerramienta herramienta plomero = (agregarHerramienta . pagarHerramienta) herramienta plomero
    |otherwise = plomero


--Parte 2

longitudMayorA :: Number ->Reparacion -> Bool
longitudMayorA x reparacion = (> x) . length . descripcionProblema

esUnGrito :: Reparacion -> Bool
esUnGrito = all isUpper . descripcionProblema

reparacionEsDificil :: Reparacion -> Bool
reparacionEsDificil reparacion = longitudMayorA 100 reparacion && esUnGrito reparacion


calcularPresupuesto :: Reparacion -> Dinero
calcularPresupuesto = (*3) . length . descripcionProblema

-- Parte 3

-- ________________________
puedeHacerReparacion :: Reparacion -> Plomero -> Bool
puedeHacerReparacion reparacion plomero = cumpleRequerimiento reparacion plomero || (tieneHerramientaConCiertaDenominacion "Martillo" plomero && esMalvado plomero)

-- Hacer generica para aplicar a distintos casos
cumpleRequerimiento :: Reparacion -> Plomero -> Bool
cumpleRequerimiento reparacion = not . null . intersect (requerimiento reparacion) . herramienta
-- ________________________

cobrarReparacion :: Number -> Plomero -> Plomero
cobrarReparacion precio plomero = plomero {
    dinero = dinero plomero + precio
    }


agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero {reparacion = reparacionplomero ++ [reparacion]}

sacarHerramientasBuenas :: Plomero -> [Herramienta]
sacarHerramientasBuenas plomero = filter (not . herramientaEsBuena)(herramienta plomero)

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas plomero = plomero {herramienta = sacarHerramientasBuenas plomero}

olvidarHerramienta :: Plomero -> Plomero
olvidarHerramienta plomero = plomero {plomero = drop 1 herramienta}

esCapazHacerReparacion :: Reparacion -> Plomero -> Plomero
esCapazHacerReparacion reparacion plomero
    |esMalvado plomero = agregarHerramienta destornillador plomero
    |reparacionEsDificil reparacion =  perderHerramientasBuenas plomero
    |otherwise = olvidarHerramienta plomero

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion plomero
    |puedeHacerReparacion reparacion plomero = (esCapazHacerReparacion . agregarReparacion . cobrarReparacion . calcularPresupuesto) reparacion plomero
    |otherwise = cobrarReparacion 100 plomero


--Parte 4

hacerReparaciones :: [Reparacion] -> Plomero -> Plomero
hacerReparaciones reparaciones plomero = foldl hacerReparacion plomero reparaciones

cantReparaciones :: Plomero -> Number
cantReparaciones plomero = length . historialReparaciones

buscarPlomeroMaxSegunCriterio :: (Plomero -> Number)-> Plomero -> Plomero -> Plomero
buscarPlomeroMaxSegunCriterio criterio plomero1 plomero2 
    |criterio plomero1 >= criterio plomero2 = plomero1
    |otherwise = plomero2

maxPlomeroSegunCriterio :: ((Plomero -> Number)-> Plomero -> Plomero -> Plomero) -> [Plomero] -> Plomero
maxPlomeroSegunCriterio criterio (p1 : ps) = foldl criterio p1 ps

empleadoMasReparador :: [Plomero] -> Plomero
empleadoMasReparador (p1 : ps) = maxPlomeroSegunCriterio (buscarPlomeroMaxSegunCriterio cantReparaciones) p1 ps

empleadoMasAdinerado :: [Plomero] -> Plomero
empleadoMasAdinerado (p1 : ps) = maxPlomeroSegunCriterio (buscarPlomeroMaxSegunCriterio dinero) p1 ps

empleadoConMasPlataInvertida :: [Plomero] -> Plomero
empleadoConMasPlataInvertida (p1 : ps) = maxPlomeroSegunCriterio (buscarEmpleadoSegunCriterio sumaDineroHerramientas) p1 ps

sumaDineroHerramientas :: Plomero -> Number
sumaDineroHerramientas plomero = sum (map precio (herramienta plomero))
