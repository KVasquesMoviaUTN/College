module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do

  describe "Test de cuantoDueleVerLasBuenas " $ do
    it "espero un False para el grupo 1" $ do
      cuantoDueleVerLasBuenas grupo1DeAmigos `shouldBe` False
    it "espero un True para el grupo 1" $ do      
      cuantoDueleVerLasBuenas grupo2DeAmigos `shouldBe` True

  describe "Test de nivelTotalDeAmsiedad" $ do
    it "espero un 100 para el grupo 1" $ do
      nivelTotalDeAmsiedad  grupo1DeAmigos `shouldBe` 100
    it "espero un 243 para el grupo 2" $ do
      nivelTotalDeAmsiedad  grupo2DeAmigos `shouldBe` 243

  describe "Test de losMasCriticados" $ do
    it "Amsiedad mayor a 50" $ do
      losMasCriticados ((>50) . amsiedad) grupo1DeAmigos `shouldBe` ["kalil","valen"]
    it "Nivel de energia par" $ do
      losMasCriticados (even . energia) grupo1DeAmigos `shouldBe` ["kalil","valen"]

  describe "Test Tareas" $ do    
    it "efectoComunTarea" $ do
      amsiedad (efectoComunTarea (grupo1DeAmigos !! 1)) `shouldBe` 70
    it "codearUnProyectoNuevo" $ do
      amsiedad (codearUnProyectoNuevo (grupo1DeAmigos !! 1)) `shouldBe` 120
    it "hacerTramitesEnAfip" $ do
      amsiedad (hacerTramitesEnAfip 5 (grupo1DeAmigos !! 1)) `shouldBe` 390
    it "andarEnBici" $ do
      amsiedad (andarEnBici 17 (grupo1DeAmigos !! 1)) `shouldBe` 0
    it "escucharMusica" $ do
      amsiedad (escucharMusica (grupo1DeAmigos !! 1)) `shouldBe` 60

  describe "Test de Energia Resultante" $ do    
    it "Aplico 2 tareas y espero 340" $ do
      energiaResultante (grupo2DeAmigos !! 1) [andarEnBici 3, hacerTramitesEnAfip 2] `shouldBe` 340
    it "Aplico 2 tareas y espero 260" $ do
      energiaResultante (grupo1DeAmigos !! 3) [escucharMusica, codearUnProyectoNuevo] `shouldBe` 260

  describe "Test de Hice lo que Pude" $ do    
    it "Codea un proyecto pero no tiene energia para afip (tiene que tener 340 de energia)" $ do
      energia (hiceLoQuePude(grupo1DeAmigos !! 0) [codearUnProyectoNuevo, hacerTramitesEnAfip 5]) `shouldBe` 340
    it "Aplico 3 tareas, tiene que tener 290 de amsiedad" $ do
      amsiedad (hiceLoQuePude(grupo2DeAmigos !! 0) [andarEnBici 10, escucharMusica, hacerTramitesEnAfip 10]) `shouldBe` 290