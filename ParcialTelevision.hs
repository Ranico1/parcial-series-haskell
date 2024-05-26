

data Serie = Serie {
nombreSerie :: String,
actores :: [Actor],
presupuesto :: Int,
cantTemporadas :: Int,
rating :: Float,
cancelada :: Bool,
bienestar :: Int
}

data Actor = Actor {
nombreActor :: String, 
sueldoPretendido :: Int, 
restriccionesTrabajo :: [String]
}

johnnyDepp = Actor{
nombreActor = "johnny depp",
sueldoPretendido = 2000000,
restriccionesTrabajo = []
}

helena = Actor{
nombreActor = "helena",
sueldoPretendido = 1500000,
restriccionesTrabajo = []
}

data Productor = Productor {
tarea :: [Serie -> Serie]
}

timBurton = Productor {
tarea = [acomodo [johnnyDepp, helena] ]
}

gatopardeitor = Productor {
    tarea = [noHacerNada] 
}

estireitor = Productor {
    tarea = [estirarDuracion] 
}

desespereitor = Productor {
    tarea = [noHacerNada, estirarDuracion]
}

canceleitor = Productor {
tarea = [cancelar 5.3]
}




-- PUNTO 1 - A (Saber si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo que quieren cobrar todos los actores.)
laSerieEstaEnRojo :: Serie -> Bool 
laSerieEstaEnRojo unaSerie = presupuesto unaSerie < (sum . sueldoActores . actores ) unaSerie

sueldoActores :: [Actor] -> [Int]
sueldoActores = map sueldoPretendido 

--PUNTO 1 B (Saber si una serie es problemática, esto ocurre si tienen más de 3 actores o actrices con más de 1 restricción)
--VERSION CASI SIN DELEGAR

--esProblematica :: Serie -> Bool 
--esProblematica = (>3) . length . cantidadDeRestricciones . actores  

--cantidadDeRestricciones :: [Actor] -> [Actor]
--cantidadDeRestricciones = filter ( (>1) . length . restriccionesTrabajo ) 

--VERSION CON MAS DELEGACION 

esProblematica :: Int -> Serie -> Bool 
esProblematica cantRestricciones unaSerie =  (length.actoresConMuchasRestricciones cantRestricciones)  (actores unaSerie)   > 3

actoresConMuchasRestricciones :: Int -> [Actor] -> [Actor] 
actoresConMuchasRestricciones cantRestricciones = filter (restriccionesPorActor cantRestricciones)

restriccionesPorActor :: Int -> Actor -> Bool 
restriccionesPorActor cantRestricciones unActor = (length . restriccionesTrabajo) unActor > cantRestricciones

--PUNTO 2 

-- ============= Funcion a reutilizar =================
evaluacionProductor:: Productor -> Serie -> Serie 
evaluacionProductor unProductor unaSerie = foldl (\x f -> f x)  unaSerie (tarea unProductor)
-- ===================================================

--B 
acomodo :: [Actor] -> Serie -> Serie
acomodo  actoresAcomodados unaSerie = unaSerie { actores = ((actoresAcomodados ++) . sacarDelReparto) (actores unaSerie)}

sacarDelReparto :: [Actor] -> [Actor]
sacarDelReparto = drop 2   

--C
noHacerNada :: Serie -> Serie 
noHacerNada unaSerie = unaSerie 

--D
estirarDuracion :: Serie -> Serie 
estirarDuracion unaSerie = unaSerie {cantTemporadas = ((2*) . cantTemporadas) unaSerie }

--F 
cancelar :: Float -> Serie -> Serie 
cancelar cifraDeRating unaSerie = unaSerie {cancelada = controlarEstado cifraDeRating unaSerie}

controlarEstado :: Float -> Serie -> Bool 
controlarEstado cifraDeRating unaSerie = laSerieEstaEnRojo unaSerie || rating unaSerie < cifraDeRating

--PUNTO 4 ======================================================

calculoBienestar :: Serie -> Serie 
calculoBienestar unaSerie 
    | cancelada unaSerie = modificarBienestar 0 unaSerie 
    | otherwise = modificarBienestar 1 unaSerie

criterio1 :: Serie -> Int 
criterio1 unaSerie 
    |cantTemporadas unaSerie > 4 = 5
    |otherwise = (10 - cantTemporadas unaSerie) *2  

criterio2 :: Serie -> Int 
criterio2 unaSerie 
    | (length . actores) unaSerie <  10  =  3
    | otherwise =  length. filter (restriccionesPorActor 2) $ actores unaSerie 


modificarBienestar :: Int -> Serie -> Serie
modificarBienestar num unaSerie = unaSerie { bienestar = (criterio1 unaSerie + criterio2 unaSerie) * num} 


-- PUNTO 6

esControvertida :: Serie -> Bool 
esControvertida unaSerie = sueldosMenorMayor (actores unaSerie)

sueldosMenorMayor :: [Actor] -> Bool 
sueldosMenorMayor (x:y:z) = sueldoPretendido x < sueldoPretendido y && sueldosMenorMayor (y:z)