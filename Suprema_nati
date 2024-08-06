module Library where
import PdePreludat
import Data.List (intersect)


type Tema = String
type Presupuesto = Number
type GruposApoyan = [String]
type Agenda = [Tema]
type Criterio =  Ley -> Bool

agenda :: Agenda
agenda = ["Uso Medicinal del Cannabis","Educación Superior"]

-- Leyes

data Ley = UnaLey {
    tema :: Tema,
    presupuesto :: Presupuesto,
    gruposApoyan :: GruposApoyan
}deriving (Show, Eq)


cannabis :: Ley
cannabis = UnaLey {
    tema = "Uso Medicinal del Cannabis",
    presupuesto = 5,
    gruposApoyan = ["Partido Cambio de Todos", "Sector Financiero"]
}
    
educacionSuperior :: Ley
educacionSuperior = UnaLey {
    tema = "Educación Superior",
    presupuesto = 30,
    gruposApoyan = ["Docentes universitarios", "Partido de centro federal"]
}

profesionalizacion :: Ley
profesionalizacion = UnaLey{
    tema = "Profesionalización del tenista de mesa",
    presupuesto = 1,
    gruposApoyan = ["Partido de Centro Federal", "Liga de Deportistas Autónomos", "Club paleta Veloz"]
}

tenis :: Ley 
tenis = UnaLey {
    tema = "Tenis",
    presupuesto = 2,
    gruposApoyan = ["Deportistas Autónomos"]
}

--Jueces
data Juez = UnJuez {
    criterioLey :: Criterio
}deriving (Show, Eq)

type CorteSuprema = [Juez]
corteSuprema :: CorteSuprema
corteSuprema = [juezOpinionPublica, juezSectorFinanciero, juezPresupuesto, juezPresupuestoTolerante, juezPartidoConservador]

juezOpinionPublica = UnJuez {criterioLey = criterioOpinionpublica agenda}

juezSectorFinanciero = UnJuez {criterioLey = criterioSectorFinanciero}

juezPresupuestoMedio = UnJuez {criterioLey = criterioPresupuesto 10}

juezPresupuestoTolerante = UnJuez {criterioLey = criterioPresupuesto 20}

juezPartidoConservador = UnJuez {criterioLey = criterioPartidoConservador}

juezAfirmativo = UnJuez {criterioLey = criterioAfirmativo}

juezLeySinApoyo = UnJuez {criterioLey = criterioSinApoyo}

juezNoTolerante = UnJuez {criterioLey = criterioPresupuesto 5}


-- Funcion 1

interseccionGrupos :: Ley -> Ley -> Bool
interseccionGrupos ley1 ley2 = (not. null) (intersect(gruposApoyan ley1) (gruposApoyan ley2))


temaContenido :: Ley  ->Ley  -> Bool
temaContenido ley1 ley2 = comprobarTemaContenido (tema ley1) (tema ley2) || comprobarTemaContenido (tema ley2) (tema ley1)

comprobarTemaContenido :: Tema -> Tema -> Bool
comprobarTemaContenido [] _ = True
comprobarTemaContenido _ [] = False
comprobarTemaContenido (t1 : t1s) (t2 : t2s)
    |t1 == t2 = comprobarTemaContenido t1s t2s
    |otherwise = comprobarTemaContenido (t1 : t1s) t2s


sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles ley1 ley2 = interseccionGrupos ley1 ley2 && temaContenido ley1 ley2

-- Funcion 2

criterioOpinionpublica ::  Agenda -> Criterio
criterioOpinionpublica agenda ley = elem (tema ley) agenda

criterioSectorFinanciero :: Criterio
criterioSectorFinanciero ley = elem "Sector Financiero" (gruposApoyan ley)

criterioPresupuesto :: Number -> Criterio
criterioPresupuesto num  = (< num) . presupuesto

criterioPartidoConservador :: Criterio
criterioPartidoConservador ley = elem "Partido Conservador" (gruposApoyan ley) 

criterioAfirmativo :: Criterio
criterioAfirmativo ley = True

criterioSinApoyo :: Criterio
criterioSinApoyo  = not . null . gruposApoyan

--Funcion 3
aplicarCriterio ::  Ley -> Juez -> Bool
aplicarCriterio ley juez = criterioLey juez ley 

pasaCriterioCorte :: Ley  -> CorteSuprema -> [Bool]
pasaCriterioCorte ley corteSuprema = map (aplicarCriterio ley) corteSuprema

votosPositivos :: [Bool] -> [Bool]
votosPositivos votosPositivos = filter (==True) votosPositivos

votosNegativos :: [Bool]-> [Bool]
votosNegativos votosNegativos = filter (==False) votosNegativos


esMayoria :: [Bool] -> Bool
esMayoria votos = (length (votosPositivos votos )) > (length (votosNegativos votos))

constitucional :: Ley  -> CorteSuprema -> Bool
constitucional ley corte = esMayoria (map (aplicarCriterio ley)corte)

--Agregar funcion 3

-- Funcion 4

invertirVoto ::Juez -> Juez
invertirVoto juez = juez {criterioLey = not . criterioLey juez}

borocotizar :: CorteSuprema ->  CorteSuprema
borocotizar corte = map invertirVoto corte

-- Funcion 5

esApoyada :: [Ley] ->Ley ->Bool
esApoyada leyesApoyadas ley = elem ley leyesApoyadas

leyesVotadas :: Juez  -> [Ley] ->[Ley]
leyesVotadas juez leyes = filter (flip aplicarCriterio juez) leyes


totalApoyadas :: [Ley]  ->[Ley] -> Bool
totalApoyadas leyesVotadas leyesApoyadas = all (esApoyada leyesApoyadas) leyesVotadas
