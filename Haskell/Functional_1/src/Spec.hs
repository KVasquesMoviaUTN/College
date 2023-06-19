module Spec where
import PdePreludat
import Library
import Test.Hspec
import System.Posix.Internals (fileType)

correrTests :: IO ()
correrTests = hspec $ do
  -----TESTS FUNCIONES COMUNES-----
  describe "Test de funcion común 3" $ do
    it "Una persona que vió 3 filmaciones, vea algún género" $ do
      cantidadDeFilmacionesVistas (comedia moni nueveReinas) `shouldBe` 4
    it "Una persona que ve la odisea de los giles y tiene $1500" $ do
      plataDisponible (efectoComun pepe laOdiseaDeLosGiles) `shouldBe` 1311
    it "Una persona que ve la odisea de los giles y tiene $10" $ do
      plataDisponible (efectoComun dardo laOdiseaDeLosGiles) `shouldBe` 0
      

  describe "Funcion verFilmacion" $ do
    it "Pepe ve la filmación 'La odisea de los giles'" $ do
      plataDisponible (verFilmacion laOdiseaDeLosGiles pepe) `shouldBe` 1311
      nivelDeSatisfaccion (verFilmacion laOdiseaDeLosGiles pepe) `shouldBe` 40
      cantidadDeFilmacionesVistas (verFilmacion laOdiseaDeLosGiles pepe) `shouldBe` 4



  describe "Funcion verFilmaciones" $ do
    it "Pepe ve la filmación 'La odisea de los giles' y 'speed'" $ do
      plataDisponible (verFilmaciones pepe [laOdiseaDeLosGiles,speed]) `shouldBe` 1122
      nivelDeSatisfaccion (verFilmaciones pepe [laOdiseaDeLosGiles,speed]) `shouldBe` 140
      cantidadDeFilmacionesVistas (verFilmaciones pepe [laOdiseaDeLosGiles,speed]) `shouldBe` 5

  describe "Test de funcion 'Me quedo con los primeros'" $ do
    it "meQuedoConLosPrimeros menores a 3 de una lista que va del 1 al 100" $ do
      meQuedoConLosPrimeros (<3) [1..100] `shouldBe` [1,2]
    it "meQuedoConLosPrimeros menores a 3 de una lista compuesta por [5,1,2,3]" $ do
      meQuedoConLosPrimeros (<3) [5,1,2,3] `shouldBe` []

  -----TESTS USUARIO 1-----
  describe "Test de funcion 1 (usuario 1)--La odisea de los giles es la única película darinesca" $ do
    it "Darin no aparece, espero un False" $ do
      peliculaDarinesca armaMortal `shouldBe` False
    it "Darin no es el primero en la lista, espero un False" $ do
      peliculaDarinesca nueveReinas `shouldBe` False
    it "Darin es el primero en la lista, espero un True" $ do
      peliculaDarinesca laOdiseaDeLosGiles `shouldBe` True

  describe "Test de funcion 2 (usuario 1)" $ do
    it "La odisea de los giles sale 200" $ do
      precioBase laOdiseaDeLosGiles `shouldBe` 200
    it "Arma Mortal sale 22" $ do
      precioBase armaMortal `shouldBe` 22
    it "9 Reinas sale 124" $ do
      precioBase nueveReinas `shouldBe` 124

  describe "Test de funcion 3 (usuario 1)" $ do
    it "Una persona que aplica el género comedia y tiene nivel de satisfacción 20" $ do
      nivelDeSatisfaccion (terror 5 pepe nueveReinas) `shouldBe` 15
    it "Una persona que aplica el género comedia y tiene nivel de satisfacción 20" $ do
      nivelDeSatisfaccion (comedia pepe nueveReinas) `shouldBe` 40
    it "Una persona de nombre Pepe que aplica una comedia" $ do
      nombre (comedia pepe nueveReinas) `shouldBe` "Pepe muy alegre"

  describe "Test de funcion 4 (usuario 1)" $ do
    it "never pony con la filmación speed para el grupo de televidentes formado por Pepe y Moni" $ do
      neverPony todosVenLaPeli speed [pepe,moni]  `shouldBe` True
    it "never pony con la filmación arma mortal para el grupo de televidentes formado por Pepe y Moni" $ do
      neverPony todosVenLaPeli armaMortal [pepe,moni]  `shouldBe` False

  describe "Test de funcion 5 (usuario 1)" $ do
    it "Coky ve hasta donde le da la billetera con las filmaciones tituladas 'Indiana Jones', 'La Flor', 'Arma Mortal' y 'Speed'" $ do
      plataDisponible (hastaDondeDeLaBilletera coky [indianaJones, laFlor, armaMortal, speed])  `shouldBe` 2
    it "Coky ve hasta donde le da la billetera con las filmaciones '9 Reinas' y 'La Flor'" $ do
      plataDisponible (hastaDondeDeLaBilletera coky [nueveReinas, laFlor])  `shouldBe` 50

  describe "Test de funcion 6 (usuario 1)" $ do
    it "laPulenta para coky con un nivel de satisfacción 200 y la filmación 'La odisea de los giles'" $ do
      laPulenta coky 200 laOdiseaDeLosGiles `shouldBe` True
    it "laPulenta para coky con un nivel de satisfacción 300 y la filmación 'La odisea de los giles'" $ do
      laPulenta coky 300 laOdiseaDeLosGiles `shouldBe` False 

-------------------------------------------------------

  describe "Test de funcion  (usuario 3) remanija" $ do
    it "una persona mira una serie de peliculas y presenta un incremento crecientes en el nivel de satisfaccion" $ do
      remanija coky [armaMortal,nueveReinas,speed] `shouldBe` True
    it "una persona mira una serie de peliculas y presenta cambios no crecientes en el nivel de satisfaccion" $ do
      remanija coky [nueveReinas,armaMortal,speed] `shouldBe` False
    it "una persona no recibe ninguna lista de peliculas " $ do
      remanija coky [] `shouldBe` False
  
  describe "Test de funcion  (usuario 3) describe show the money" $ do
    it "se considera si una persona posee sufiente FONDOS para el consumo de una filmacion" $ do
      tiendateca laOdiseaDeLosGiles moni `shouldBe` True
    it "se considera si una persona NO posee sufiente FONDOS para el consumo de una filmacion" $ do
      tiendateca laOdiseaDeLosGiles coky `shouldBe` False
  
  {-describe "Test de funcion  (usuario 3) DameDos" $ do
    it "una persona elige peliculas que sean daniresca " $ do
      damedos [nueveReinas, laOdiseaDeLosGiles, elSecretoDeSusOjos] `shouldBe` [laOdiseaDeLosGiles, elSecretoDeSusOjos] 
    it "una persona elige peliculas que sean daniresca " $ do
      damedos [nueveReinas, elSecretoDeSusOjos] `shouldBe` [elSecretoDeSusOjos]
    it "una persona elige peliculas que sean daniresca " $ do
      damedos [nueveReinas, laFlor] `shouldBe` []
-}


-------------------------------------------------
-- TESTS USUARIO 2
{-
describe "Combo vendible (integrante 2)" $ do    
    it "cumple la condicion de que pepe y moni tienen una cantidad mayor de dinero al precio total de alguna de las filmaciones" $ do
      comboVendible [speed, nueveReinas] [moni, pepe] `shouldBe` True
    it "No cumple la condicion tal que coky no tiene una cantidad mayor de dinero al precio total de alguna de las filmaciones" $ do
      comboVendible [speed, nueveReinas] [moni, pepe, coky] `shouldBe` False

describe "iCanGetNo (integrante 2)" $ do    
    it "I can´t get no para “La Odisea de los giles” con el grupo de televidentes Moni y Coky con un nivel de 150 de satisfacción" $ do
      iCanGetNo laOdiseaDeLosGiles [moni, coky] 150 `shouldBe` [coky]
    it "I can´t get no para “La Odisea de los giles” con el grupo de televidentes Moni y Coky con un nivel de 150 de satisfacción" $ do
      iCanGetNo laOdiseaDeLosGiles [moni, coky] 150 `shouldBe` ??

describe "tengoCaprichitoCon (integrante 2)" $ do    
    it "tengoCaprichitoCon con la calificación 6, 8 y 9 para la filmación “La odisea de los giles" $ do
      tengoCaprichitoCon [6, 8, 9] laOdiseaDeLosGiles `shouldBe` True
    it "tengoCaprichitoCon con la calificación 6, 7 y 9 para la filmación “La odisea de los giles”" $ do
      tengoCaprichitoCon [6, 7, 9] laOdiseaDeLosGiles `shouldBe` False
-}
{-
  describe "Test de funcion 2 Precio extra (Integrante 2)" $ do
    it "La odisea de los giles, 116 minutos" $ do
      precioExtra laOdiseaDeLosGiles `shouldBe` 10
    it "la flor, 840 minutos" $ do
      precioExtra laFlor `shouldBe` 100
    it "9 Reinas, 114 minutos y del año 2000" $ do
      precioExtra nueveReinas `shouldBe` 50
    it "Arma mortal, 109 minutos y del año 1984" $ do
      precioExtra armaMortal `shouldBe` 0

      -- test del minutos exedentes
  describe "Test de funcion minutos exedentes (usuario 3)" $ do
    it "minutos exedentes de odisea de los giles espero un uno" $ do
      minExcedentes laOdiseaDeLosGiles `shouldBe` 1
    it "minutos exedentes de arma mortal" $ do
      minExcedentes armaMortal `shouldBe` 6

  describe "Test de funcion  (usuario 3) precio total" $ do
    it "precio total de odisea de los giles espero 189" $ do
      precioTotal laOdiseaDeLosGiles `shouldBe` 189
    it "precio total de arma mortal estero 22 " $ do
      precioTotal armaMortal `shouldBe` 22

  describe "Test de funcion  (usuario 3) genero aventura" $ do
    it "si la pelicula es mala no debo esperar cambios en el nivel de satisfaccion" $ do
      nivelDeSatisfaccionpepe `shouldBe` 20
    it "si la pelicula es buena se debe aumentar el nivel de satifeccion igual que comedia" $ do
      nivelDeSatisfaccion(comedia(personas !!1)indianaJonesIV) `shouldBe` 40

  describe "Test de funcion  (usuario 3) genero tragicomedia" $ do
    it "si la persona aplica tragicomedia a odisea de los giles" $ do
      edad (tragicomedia pepelaOdiseaDeLosGiles)`shouldBe` 31
    it "si la persona aplica tragicomedia ala odisea de los giles" $ do
      nivelDeSatisfaccion(tragicomedia(personas !!1)laOdiseaDeLosGiles) `shouldBe` 6
    it "si la persona aplica tragicomedia ala odisea de los giles" $ do
      nombre(tragicomedia(personas !!1)laOdiseaDeLosGiles) `shouldBe` "Pepe muy alegre" 
-}
