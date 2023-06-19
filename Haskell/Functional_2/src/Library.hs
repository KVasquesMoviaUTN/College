module Library where
import PdePreludat

--PUNTO 1
type Tarea = Persona -> Persona

data Persona = Persona {
  nombre :: String,
  edad :: Number,
  energia :: Number,
  alegria :: Number,
  amsiedad :: Number,
  cantidadDeTareas :: Number
}deriving (Show)

estres :: Persona -> Number
estres persona
  |cantidadDeTareas persona <= 5 = amsiedad persona
  |otherwise = 1.5 * amsiedad persona

definirNivelDeEnergia :: Persona -> Number
definirNivelDeEnergia persona
  |alegria persona > amsiedad persona = 340 `min` (2* alegria persona)
  |amsiedad persona > alegria persona && (not . esJovato) persona = 300 - estres persona
  |otherwise = alegria persona + 10

--PUNTO 2
grupo1DeAmigos :: [Persona]
grupo1DeAmigos =[--Nombre--edad--energia--alegria--amsiedad--tareas
  Persona "kalil" 20 0 75 80 6,
  Persona "valen" 21 0 93 80 8,
  Persona "maxi" 31 0 88 50 7,
  Persona "impostor" 45 0 20 100 3
  ]

grupo2DeAmigos :: [Persona]
grupo2DeAmigos =[--Nombre--edad--energia--alegria--amsiedad--tareas
  Persona "lean" 16 0 231 728 4,
  Persona "mati" 47 0 144 143 6,
  Persona "axel" 21 0 88 1200 5,
  Persona "amigo4" 68 0 120 100 78
  ]

esJovato :: Persona -> Bool
esJovato = (40<=) . edad

cuantoDueleVerLasBuenas :: [Persona] -> Bool
cuantoDueleVerLasBuenas = all ((>100).definirNivelDeEnergia) . filter esJovato

nivelTotalDeAmsiedad :: [Persona] -> Number
nivelTotalDeAmsiedad = (sum . map amsiedad) . filter esJovato

losMasCriticados :: (Persona -> Bool) -> [Persona] -> [String]
losMasCriticados criterio = take 2 . map nombre . filter criterio

efectoComunTarea :: Tarea
efectoComunTarea persona = persona{amsiedad = 0 `max` (amsiedad persona - 10), energia = definirNivelDeEnergia persona, cantidadDeTareas = cantidadDeTareas persona - 1}

codearUnProyectoNuevo :: Tarea
codearUnProyectoNuevo persona = efectoComunTarea persona{alegria = alegria persona + 110, amsiedad = amsiedad persona + 50}

hacerTramitesEnAfip :: Number -> Tarea
hacerTramitesEnAfip malditosTramites persona = efectoComunTarea persona{amsiedad = 300 `max` (malditosTramites * amsiedad persona)}

andarEnBici :: Number -> Tarea
andarEnBici kmAndados persona = efectoComunTarea persona{amsiedad = 0, alegria = alegria persona + 50 * kmAndados}

escucharMusica :: Tarea
escucharMusica persona = efectoComunTarea persona{amsiedad = amsiedad persona - 10}

energiaResultante :: Persona -> [Tarea] -> Number
energiaResultante persona tareas = definirNivelDeEnergia (foldr ($) persona tareas)

hiceLoQuePude :: Persona -> [Tarea] -> Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:tareas)
  |energia (tarea persona) > 100 = hiceLoQuePude (tarea persona) tareas
  |otherwise = persona