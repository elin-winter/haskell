module Library where
import Data.List (intersect)
import PdePreludat

-- --------------------------- Dominio -----------------------
data Personaje = UnPersonaje {
    nombre :: Nombre,
    puntaje :: Puntaje, 
    inventario :: [Material]
} deriving (Eq, Show)


data Receta = UnaReceta {
    tiempo :: Tiempo, 
    materiales :: [Material],
    resultado :: Material
} deriving (Eq, Show)

data Bioma = UnBioma {
    materialesBioma :: [Material], 
    elemNecesario :: Material 
}

-- --------------------------- Definición de Tipos -----------------------
type Nombre = String
type Puntaje = Number
type Tiempo = Number
type Material = String
type Herramienta = [Material] -> Material

-- --------------------------- Modelaje (Ejemplos) -----------------------
-- Recetas
fogata :: Receta
fogata = UnaReceta 10 ["Madera", "Fosforo"] "Fogata"

polloAsado :: Receta
polloAsado = UnaReceta 300 ["Fogata", "Pollo"] "Pollo Asado"

sueter :: Receta
sueter = UnaReceta 600 ["Lana", "Agujas", "Tintura"] "Sueter"

-- Biomas
artico :: Bioma
artico = UnBioma ["Hielo", "Iglus", "Lobos"] "Sueter"

-- --------------------------- Funciones -----------------------
-- ------------ Parte 1
creaftearObjeto :: Receta -> Personaje -> Personaje
creaftearObjeto receta personaje
    | puedeCraftear (materiales receta) personaje = hacerObjeto receta personaje
    | otherwise = modificarPuntos (-100) personaje 

puedeCraftear :: [Material] -> Personaje -> Bool
puedeCraftear materiales = not . null . intersect materiales . inventario

hacerObjeto :: Receta -> Personaje -> Personaje
hacerObjeto receta = 
    agregarInventario (resultado receta) . inventarioNuevo receta . 
    modificarPuntos (obtenerPuntosReceta receta)

modificarPuntos :: Number -> Personaje -> Personaje
modificarPuntos delta personaje = personaje { puntaje = puntaje personaje + delta} 

agregarInventario :: Material -> Personaje -> Personaje
agregarInventario material personaje = personaje {inventario = material : inventario personaje}

inventarioNuevo :: Receta -> Personaje -> Personaje
inventarioNuevo receta personaje = personaje {inventario = usarMateriales receta personaje}

usarMateriales :: Receta -> Personaje -> [Material]
usarMateriales receta personaje = filter (`notElem` materiales receta) (inventario personaje)

obtenerPuntosReceta :: Receta -> Puntaje
obtenerPuntosReceta receta = tiempo receta * 10

-- ------------ Parte 2
-- Funcion 1
recetasDuplicanPuntaje :: Personaje -> [Receta] -> [Receta]
recetasDuplicanPuntaje personaje = filter (duplicanPuntos personaje)

duplicanPuntos :: Personaje -> Receta -> Bool
duplicanPuntos personaje receta =  puntaje (creaftearObjeto receta personaje) >= puntaje personaje *2

-- Funcion 2
craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldl (flip creaftearObjeto) 

-- Funcion 3
masPuntosReversa :: Personaje -> [Receta] -> Bool
masPuntosReversa personaje recetas = 
    puntosDespuesCraft personaje recetas < puntosDespuesCraft personaje (reverse recetas)

puntosDespuesCraft :: Personaje -> [Receta] -> Puntaje
puntosDespuesCraft personaje recetas = puntaje (craftearSucesivamente personaje recetas)

-- ------------ Parte 3
-- Funcion 1
minarBioma :: Bioma -> Personaje -> Herramienta -> Personaje
minarBioma bioma personaje herramienta
    | puedeMinar bioma personaje = conseguirMateriales bioma personaje herramienta
    | otherwise = personaje

puedeMinar :: Bioma -> Personaje -> Bool
puedeMinar bioma = puedeCraftear [elemNecesario bioma]

conseguirMateriales :: Bioma -> Personaje -> Herramienta -> Personaje
conseguirMateriales bioma personaje herramienta = (modificarPuntos 50 . agregarInventario (herramienta (materialesBioma bioma))) personaje

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Number -> Herramienta
pico = flip (!!) 

motocierra :: Herramienta
motocierra = head . filter ((<3) . length)

martillo :: Herramienta
martillo = flip (!!) 4

tijeras :: Herramienta
tijeras lista =  (!!) lista (indiceMedio lista)

indiceMedio :: [Material] -> Number
indiceMedio = flip div 2 . length

sincel :: Herramienta
sincel = \lista -> (!!) lista 3

-- ------------ Parte 4
{-
¿Qué pasa al intentar minar en un bioma con infinitos materiales? 
Mostrar ejemplos donde con diferentes herramientas o personajes
sucedan diferentes cosas. Justificar. 

Va a depender enteramente de la herramienta con la cual se mine. 
Si utilizamos una herramienta que evalua una condición especifica, eligiendo
el primero de la lista por ejemplo, entonces la función minar se podrá aplicar
y obtendremos un resultado. Esto se debe al motor de evaluación diferida de Haskell
que toma en cuenta primero las funciones, luego los operandos. Como nota que solo
necesita el primero de la lista, no desperdicia recursos en conocer el resto de la lista
y devuelve ese resultado. 

Ahora, si la herramienta requiere evaluar la lista completa, sin excepción, por ejemplo
motocierra que requiere la longitud de la lista, entonces la función minar en ese caso
nunca va poder devolver un resultado. 
-}
