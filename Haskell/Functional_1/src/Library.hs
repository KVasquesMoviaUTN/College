module Library where
  
import PdePreludat
import Data.List (find)

type Genero = Persona -> Filmacion -> Persona

data Filmacion = Filmacion{
  titulo :: String,
  puntaje :: Number,--Va de 0 a 10
  duracionEnMinutos :: Number,
  añoDeFilmacion :: Number,
  listaDeActoresQueParticipan :: [String],
  genero :: Genero
}deriving(Show,Eq,Ord)

data Persona = Persona{
  nombre :: String,
  nivelDeSatisfaccion :: Number,--Puede ser negativo
  edad :: Number,
  cantidadDeFilmacionesVistas :: Number,
  plataDisponible :: Number
}deriving(Show,Eq,Ord)

armaMortal :: Filmacion
armaMortal = Filmacion "Arma Mortal" 7 109 1987 ["Mel Gibson","Danny Glover","Gary Busey"] accion

nueveReinas :: Filmacion
nueveReinas = Filmacion "9 Reinas" 8 114 2000 ["Gaston Pauls","Ricardo Darin","Leticia Bredice","Pochi Ducasse"] (drama 5)

laOdiseaDeLosGiles :: Filmacion
laOdiseaDeLosGiles = Filmacion "La odisea de los giles" 8 116 2019 ["Ricardo Darin","Luis Brandoni","Verónica Llinás","Daniel Aráoz","Rita Cortese"] comedia

laFlor :: Filmacion
laFlor = Filmacion "La Flor" 7 840 2018 ["Pilar Gamboa"] tragicomedia

speed :: Filmacion
speed = Filmacion "Speed" 7 116 1994 ["Keanu Reeves","Sandra Bullock","Dennis Hopper","Jeff Daniels","Alan Ruck"] accion

indianaJonesIV :: Filmacion
indianaJonesIV = Filmacion "Indiana Jones IV" 6 125 2007 ["Harrison Ford"] (aventura "IV")

indianaJones :: Filmacion
indianaJones = Filmacion "Indiana Jones" 8 115 1981 ["Harrison Ford"] (aventura "IV")

elSecretoDeSusOjos :: Filmacion
elSecretoDeSusOjos = Filmacion "El secreto de sus ojos" 9 129 2009 ["Ricardo Darin","Soledad Villamil"] (drama 3)

--------------------sat-age-cant-money---
moni :: Persona
moni = Persona "Moni" 50 31 3 5600

pepe :: Persona
pepe = Persona "Pepe" 20 30 3 1500

dardo :: Persona
dardo = Persona "pers3" 4 77 2 10

coky :: Persona
coky = Persona "Coky" 120 20 40 50

mickJagger :: Persona
mickJagger = Persona "Mick" 200 79 10 10000

--FUNCIONES TP1-----------

peliculaDarinesca :: Filmacion -> Bool
peliculaDarinesca = ("Ricardo Darin" ==) . head . listaDeActoresQueParticipan

pintaBuena :: Filmacion -> Bool--(integrante 2)
pintaBuena = (>4) . length . listaDeActoresQueParticipan

minExcedentes :: Filmacion -> Number--(integrante 3)
minExcedentes = abs . (+ (-115)). duracionEnMinutos

esVieja :: Filmacion -> Bool
esVieja = (< 1990) . añoDeFilmacion

precioBase :: Filmacion -> Number
precioBase peli 
  |pintaBuena peli = 200
  |esVieja peli = ((2*) . length . titulo) peli
  |otherwise = 100 + ((3*) . puntaje) peli

precioExtra :: Filmacion -> Number
precioExtra peli
  |duracionEnMinutos peli > 115 = min 100 (10 * minExcedentes peli)
  |(not . esVieja) peli = 50
  |otherwise = 0

precioTotal :: Filmacion -> Number
precioTotal peli
  |precioBase peli + precioExtra peli <= 200 = precioBase peli + precioExtra peli
  |otherwise = (precioBase peli + precioExtra peli) * 0.9

--GENEROS-----------------------

efectoComun :: Genero
efectoComun persona peli = persona{
  cantidadDeFilmacionesVistas = cantidadDeFilmacionesVistas persona + 1,
  plataDisponible = max (plataDisponible persona - precioTotal peli) 0}  

terror :: Number -> Genero
terror cantidadDeLitrosDeSangre persona = efectoComun (persona{nivelDeSatisfaccion = nivelDeSatisfaccion persona - cantidadDeLitrosDeSangre})

accion :: Genero
accion persona peli 
  |pintaBuena peli = efectoComun (persona{nivelDeSatisfaccion = nivelDeSatisfaccion persona + 100}) peli
  |otherwise = efectoComun persona peli

drama :: Number -> Genero
drama escenasFelices persona = efectoComun (persona{edad = edad persona + 1, nivelDeSatisfaccion = min 3 escenasFelices})

comedia :: Genero--Testeada (funciona bien)
comedia persona = efectoComun (persona{nivelDeSatisfaccion = nivelDeSatisfaccion persona * 2, nombre = nombre persona ++ " muy alegre"})

tragicomedia :: Genero
tragicomedia persona peli = comedia (drama 4 persona peli) peli

aventura :: String -> Genero 
aventura sagaMala persona peli
  |esMala sagaMala peli = efectoComun persona peli
  |otherwise = comedia persona peli

esMala :: String -> Filmacion -> Bool
esMala sagaMala = (== sagaMala) . last . words . titulo

-------------FUNCIONES TP2-------------

verFilmacion :: Filmacion -> Persona -> Persona
verFilmacion peli persona = genero peli persona peli

verFilmaciones :: Persona -> [Filmacion] -> Persona
verFilmaciones persona (peli:pelis) = foldl (flip verFilmacion) persona (peli:pelis)

todosVenLaPeli :: Filmacion -> [Persona] -> [Number]--Para Never Pony
todosVenLaPeli peli = map (nivelDeSatisfaccion . verFilmacion peli)

neverPony :: (a->[b]->[Number]) -> a -> [b] -> Bool
neverPony todosVenLaPeli peli = (>100) . maximum . todosVenLaPeli peli

hastaDondeDeLaBilletera :: Persona -> [Filmacion] -> Persona
hastaDondeDeLaBilletera persona [] = persona
hastaDondeDeLaBilletera persona (peli:pelis)
  |plataDisponible persona >= precioTotal peli = hastaDondeDeLaBilletera (verFilmacion peli persona) pelis
  |otherwise = hastaDondeDeLaBilletera persona pelis

meQuedoConLosPrimeros :: (a -> Bool) -> [a] -> [a]
meQuedoConLosPrimeros criterio = foldr (\elemento elementos -> if criterio elemento then (elemento:elementos)  else []) []
--Usamos foldr para que quede definido el caso default, y no usamos el foldl
--Cuando usamos foldr, la función de plegado se aplica desde el último elemento de la lista hasta el primero, lo que permite construir la nueva lista en el orden deseado, agregando los elementos al comienzo. Si utilizáramos foldl, la función de plegado se aplicaría desde el primer elemento hasta el último, lo que generaría una lista con los elementos en orden inverso.

laPulenta  :: Persona -> Number -> Filmacion -> Bool
laPulenta persona nivelSat peli = ((>nivelSat).nivelDeSatisfaccion)(verFilmacion peli persona)
--meQuedoConLosPrimeros (laPulenta mickJagger 300 [indianaJones,armaMortal,nueveReinas,speed])-Funcion que dado un nivel de satisfaccion objetivo, devuelve una lista con las primeras pelis que le van a gustar a la persona

-- FUNCIONES INTEGRANTE 2

comboVendible :: [Filmacion] -> [Persona] -> Bool
comboVendible pelis = any (tienePlataSuficiente pelis)

tienePlataSuficiente :: [Filmacion] -> Persona -> Bool
tienePlataSuficiente pelis persona = any ((< plataDisponible persona) . precioTotal) pelis

iCanGetNo :: Filmacion -> [Persona] -> Number -> [Persona]
iCanGetNo _ [] _ = []  --Caso base: no quedan más personas
iCanGetNo peli (persona:personas) nivelMinimo
  | nivelDeSatisfaccion (verFilmacion peli persona) > nivelMinimo = persona : iCanGetNo peli personas nivelMinimo
  | otherwise = iCanGetNo peli personas nivelMinimo

--tengoCaprichitoCon :: [Number] -> Filmacion -> Bool
--tengoCaprichitoCon puntaje filmacion = puntaje filmacion elem puntaje

-- remnija !!
remanija :: Persona -> [Filmacion] -> Bool
remanija persona [] = False
remanija persona [_] = True
remanija persona (pelix:peliy:pelis) = nivelDeSatisfaccion (verFilmacion pelix persona)< nivelDeSatisfaccion (verFilmacion peliy persona) && remanija persona pelis

-- dos peliculas darinesca
damedos:: [Filmacion] -> [Filmacion] 
damedos listadepelis = take 2 (filter peliculaDarinesca listadepelis)


-- show me the money !!

tiendateca:: Filmacion -> Persona -> Bool
tiendateca peli = (> precioTotal peli) . plataDisponible  
