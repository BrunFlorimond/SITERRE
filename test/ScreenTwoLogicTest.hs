module ScreenTwoLogicTest where

import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           Test.Hspec
import           DataTest
import           ScreenTwoLogic
import           ScreenOneLogic



testScreen2 :: IO ()
testScreen2 = hspec $ do
  
    describe "test de Verifications de présences de materiau-mélange / materiau-gisement" $ do
           it "retourne False s'il n'y a pas de gisement" $ do
             shouldBe (verifierPresence carteVide (fromJust (head <$> listeMelange))) False
           it "retourne False s'il n'y as pas de matériau résultat" $ do
             shouldBe (verifierPresence carteGi1Gi2 ([ingredientOK01,ingredientOK02],Nothing)) False
           it "retourne False s'il n'y a pas de materiau à mélanger" $ do
             shouldBe (verifierPresence carteGi1Gi2 ([],Just materiau1)) False
           it "retourne False si l'un des ingredients n'a pas de champs Nom" $ do
             shouldBe (verifierPresence carteGi1Gi2 ([ingredientOK01,ingredient1',ingredientOK03],Just materiau1)) False
           it "retourne Just True si chaque ingredient a son gisement" $ do
             shouldBe (verifierPresence carteGi1Gi2 ([ingredientOK,ingredientOK02],Just materiau1)) True
           it "retourne Just False car un gisement manque" $ do
             shouldBe (verifierPresence carteGi1Gi3 ([ingredientOK,ingredientOK02],Just materiau1)) False
             
    describe "test de la fonction de calcul du nombre d'aller retour a effectuer" $ do
           it "retourne le bon resutat avec une quantite en volume et la densite (forte) disponible dans  le materiau" $ do
             shouldBe (nbAllerRetours (Volume 200) camion1 materiauDense) 67 
           it "retourne le bon resultat avec une quantite en volume et la densite (faible) disponible dans lemateriau" $ do
             shouldBe (nbAllerRetours (Volume 200) camion1 materiauPeuDense) 10 
           it "retourne le bon resultat avec une quantite en volume et la densite non disponible dans le materiau" $ do
             shouldBe (nbAllerRetours (Volume 200) camion1 materiau1) 10 
           it "retourne le bon resutat avec une quantite en tonne et la densite (forte) disponible dans  le materiau" $ do
             shouldBe (nbAllerRetours (Tonne 200) camion1 materiauDense) 7 
           it "retourne le bon resultat avec une quantite en tonne et la densite (faible) disponible dans lemateriau" $ do
             shouldBe (nbAllerRetours (Tonne 200) camion1 materiauPeuDense) 10000 
           it "retourne le bon resultat avec une quantite en tonne et la densite non disponible dans le materiau" $ do
             shouldBe (nbAllerRetours (Tonne 200) camion1 materiau1) 7 
           
    describe "test de la fonction de calcul des données de trajets" $ do
      it "retourne Nothing si le départ est un chantier" $ do
        shouldBe (calculData (Trajet siteChantier (Distance 5 10)) qt1) Nothing
      it "retourne le bon resultat si le départ est un gisement avec un seul aller-retour" $ do
        shouldBe (calculData trajet1 qt1) (Just (DataTrajet trajet1 20 2.5 (DataSite (Tonne 5))))
      it "retourne le bon resultat si le départ est un gisement avec plusieurs allers-retours" $ do
         shouldBe (calculData trajet5 (Tonne 753)) (Just (DataTrajet trajet5 (fromIntegral $ ceiling (753/30) * 50 * 2) ((fromIntegral $ ceiling (753/30)) * (25 * 2) * (25/100)) (DataSite (Tonne 753))))
      it "retourne le bon resultat si le départ est une plateforme avec plusieurs aller-retour" $ do
        shouldBe  (calculData grandeNoria' (Tonne 68978.5879)) (Just (DataTrajet grandeNoria' (fromIntegral $ ceiling (68978.5879/30) * 20 * 2) ((fromIntegral $ ceiling (68978.5879/30)) * (10 * 2) * (25/100)) (DataPf (68978.5879*2) (68978.5879*2) (68978.5879*2))))
        
    describe "test de la fonction d'assignation de quantité à chaque gisement" $ do
     it "retourne Nothing si la liste de gisement est vide" $ do
      shouldBe (dispatch (Just $ Tonne 30) [])  Nothing
     it "retourne Nothing si la quantite necessaire est  égale a 0" $ do
      shouldBe (dispatch (Just $ Tonne 0) [trajet1,trajet2,trajet3]) Nothing
     it "retourne Nothing si la quantite necessaire est négative" $ do
      shouldBe (dispatch (Just $ Tonne (-5)) [trajet1,trajet2,trajet3]) Nothing
     it "retourne Nothing si la quantite necessaire est exprimee en Volume" $ do
      shouldBe (dispatch (Just $ Volume 30) [trajet1,trajet2,trajet3]) Nothing
     it "retourne Nothing s'il n'y a pas assez de quantite dans les gisements" $ do
      shouldBe (dispatch (Just $ Tonne 6) [trajet1]) Nothing
     it "retourne le bon resultat avec un seul gisement suffisant (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 4.35) [trajet1]) (Just [(DataTrajet trajet1 20 2.5 (DataSite (Tonne 4.35)))])
    it "retourne le bon resultat avec deux gisements suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 25.59) [trajet1,trajet1']) (sequence [calculData trajet1 (Tonne 5),calculData trajet1' (Tonne 20.59)]) 
    it "retourne le bon resultat avec trois gisements suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 25.59) [trajet1,trajet1,trajet1']) (sequence [calculData trajet1 (Tonne 5),calculData trajet1 (Tonne 5),calculData trajet1' (Tonne 15.59)]) 
    it "retourne le bon resultat avec trois gisements dont deux suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 17.50) [trajet1,trajet1',trajet1]) (sequence [calculData trajet1 (Tonne 5),calculData trajet1' (Tonne 12.50)]) 
    it "retourne Nothing avec avec 1 gisement en Volume qui n'a pas de densite" $ do
      shouldBe (dispatch (Just $ Tonne 30) [trajetSansDensite,trajetMateriauDense]) Nothing
    it "retourne le bon resultat avec un 1 gisement en volume suffisant " $ do
      shouldBe (dispatch (Just $ Tonne 30) [trajetMateriauDenseV]) (sequence [calculData trajetMateriauDenseV (Tonne 30)]) 
    it "retourne Nothing avec 1 gisement en Volume insuffisant (volume < tonne avec densite forte) " $ do
      shouldBe (dispatch (Just $ Tonne 50.5) [trajetMateriauDenseV]) Nothing
    it "retourne le bon resultat avec 2 gisements en Volume (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 55) [trajetMateriauDenseV,trajetMateriauDenseV]) (sequence [calculData trajetMateriauDenseV (Tonne 50), calculData trajetMateriauDenseV (Tonne 5)])
    it "retourne le bon resultat avec 2 gisements en Volume  insuffisants (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 105) [trajetMateriauDenseV,trajetMateriauDenseV]) Nothing
    it "retourne le bon resultat avec 2 gisements (un en volume et un en Tonne) (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 54.5) [trajetMateriauDenseV,trajet1]) (sequence [calculData trajetMateriauDenseV (Tonne 50), calculData trajet1 (Tonne 4.5)])

    describe "tests de la fonction petiteNoria qui assigne une quantite de materiau pour chaque gisement et calcul les données de trajet" $ do
     it "retourne un tuple avec une liste vide et nothing si le materiau est en Nothing" $ do
      shouldBe (petiteNoria carteGi1Gi2 (Tonne 10) ([ingredientOK,ingredientOK01],Nothing)) ([],Nothing)
     it "retourne le tuple sans information de trajet s'il n'existe pas de gisement dans la carte" $ do
      shouldBe (petiteNoria carteVide (Tonne 30) ([ingredientOK,ingredientOK01],Just materiau1)) ([(ingredientOK,Nothing),(ingredientOK01, Nothing)],Just materiau1)
     it "retourne le tuple avec une liste d'assignement gisement/quantite vide si la quantite est a 0" $ do
      shouldBe (petiteNoria carteGi1Gi2 (Tonne 0) ([ingredientOK,ingredientOK01],Just materiau1)) ([(ingredientOK,Nothing),(ingredientOK01, Nothing)],Just materiau1)
     it "retourne le tuple avec une liste d'assignement gisement/quantite vide si la quantite est a negative" $ do
      shouldBe (petiteNoria carteGi1Gi2 (Tonne (-5)) ([ingredientOK,ingredientOK01],Just materiau1)) ([(ingredientOK,Nothing),(ingredientOK01, Nothing)],Just materiau1) 
     it "retourne le tuple avec une liste d'assignement ingredient/gisement/quantite vide si la liste d'ingrédient est vide" $ do
      shouldBe (petiteNoria carteGi1Gi2 (Tonne 5) ([],Just materiau1)) ([],Just materiau1)
     it "retourne le tuple juste avec un gisement pour un ingrédient" $ do
      shouldBe (petiteNoria carteGi1 (Tonne 2) ([ingredientOK],Just materiau1)) ([(ingredientOK,sequence [calculData trajet1 (Tonne 1)])],Just materiau1)
     it "retourne le tuple juste avec deux gisements pour un ingrédient (avec distances différentes)" $ do
      shouldBe (petiteNoria (Carte [trajet1',trajet1] grandeNoria') (Tonne 15) ([ingredientOK],Just materiau1)) ([(ingredientOK,sequence [calculData trajet1 (Tonne 5),calculData trajet1' (Tonne 2.5)])],Just materiau1)
     it "retourne le tuple avec plusieurs gisements pour plusieurs ingrédients" $ do
      shouldBe (petiteNoria carteGi1Gi2 (Tonne 7) ([ingredientOK,ingredientOK01],Just materiauOK)) ([(ingredientOK,sequence [calculData trajet1 (Tonne 3.5)]),(ingredientOK01,sequence [calculData trajet2 (Tonne 3.5)])],Just materiauOK)
     it "retourne le tuple avec plusieurs gisements pour plusieurs ingrédients mais avec quantite insuffisante sur un gisement" $ do
      shouldBe (petiteNoria (Carte [trajet1,trajet1',trajet2] grandeNoria') (Tonne 15) ([ingredientOK,ingredientOK01],Just materiauOK)) ([(ingredientOK,sequence [calculData trajet1 (Tonne 5),calculData trajet1' (Tonne 2.5)]),(ingredientOK01,Nothing)],Just materiauOK)

    describe "test de la fonction d'application du materiau à la plateforme" $ do
      it " renvoie le meme trajet si le materiau est en Nothing" $ do
        shouldBe (appliquerMatPlateforme grandeNoriaSansGis Nothing)  grandeNoriaSansGis
      it "renvoie le meme trajet si le trajet ne part pas d'une plateforme" $ do
        shouldBe (appliquerMatPlateforme trajet1 (Just materiau1)) trajet1
      it " renvoie la meme plateforme si la plateforme dispose d'un gisement" $ do
        shouldBe (appliquerMatPlateforme grandeNoria' (Just materiau1))  grandeNoria'
      it " renvoie la plateforme avec le gisement incororé" $ do
        shouldBe (appliquerMatPlateforme grandeNoriaSansGis (Just materiauOK))  grandeNoria'

    describe "test de la fonction grandeNoria qui calcule les données du trajet de grande Noria" $ do
     it "retourne un tuple vide si le materiau est en nothing" $ do
        shouldBe (grandeNoria carteGi1Gi2PlSansGis (Tonne 10) (petiteNoria carteGi1Gi2 (Tonne 10) ([ingredientOK,ingredientOK01],Nothing))) ([],Nothing,Nothing)
     it "retourne un tuple avec la grande Noria calculée meme si pas de petite Noria" $ do
       shouldBe (grandeNoria carteVidePlSansGis (Tonne 30) (petiteNoria carteVide (Tonne 30) ([ingredientOK,ingredientOK01],Just materiauOK))) (fst $ petiteNoria carteVide (Tonne 30) ([ingredientOK,ingredientOK01],Just materiauOK),Just materiauOK,calculData grandeNoria' (Tonne 30))
     it "retourne un tuple vide si la quantite est  à 0" $ do
       shouldBe (grandeNoria carteGi1Gi2PlSansGis (Tonne 0) (petiteNoria carteGi1Gi2 (Tonne 0) ([ingredientOK,ingredientOK01],Just materiau1))) ([(ingredientOK,Nothing),(ingredientOK01, Nothing)],Just materiau1,Nothing)
     it "retourne un tuple vide si la quantite est negative" $ do
       shouldBe (grandeNoria carteGi1Gi2PlSansGis (Tonne (-5)) (petiteNoria carteGi1Gi2 (Tonne (-5)) ([ingredientOK,ingredientOK01],Just materiau1))) ([(ingredientOK,Nothing),(ingredientOK01, Nothing)],Just materiau1,Nothing)
     it "retourne le tuple avec une liste d'assignement ingredient/gisement/quantite vide si la liste d'ingrédient est vide mais avec la grandenoria calculée" $ do
       shouldBe (grandeNoria carteGi1Gi2PlSansGis (Tonne 5) (petiteNoria carteGi1Gi2 (Tonne 5) ([],Just materiauOK))) ([],Just materiauOK,calculData grandeNoria' (Tonne 5))
     it "retourne le tuple avec une liste d'assignement ingredient/gisement/quantite juste" $ do
       shouldBe (grandeNoria carteGi1Gi2PlSansGis (Tonne 5) (petiteNoria carteGi1Gi2PlSansGis (Tonne 5) ([ingredientOK,ingredientOK01],Just materiauOK))) (fst $ petiteNoria carteGi1Gi2PlSansGis (Tonne 5) ([ingredientOK,ingredientOK01],Just materiauOK),Just materiauOK,calculData grandeNoria' (Tonne 5))

     describe "test de la fonction de calcul de cout d'une noria" $ do
       it "calcul les couts d'un trajet de petite noria (gisement -> plateforme)" $ do
          shouldBe (calcCoutNoria params (fromJust dataTrajet1)) (TrajetCout [trajet1] 150 (2.5*1.5) (((2.5*2.6)/1000) * 5) (2.5 * 2.6) (5*5) 0 0 0 20 2.5)
       it "calcul les couts d'un trajet de grande noria (plateforme -> chantier)" $ do
         shouldBe (calcCoutNoria params (fromJust dataTrajetGNoria)) (TrajetCout [grandeNoria'] 150 (5*1.5) ((5*2.6/1000) * 5) (5*2.6) 0 10 10 10 40 5) 

     describe "test de la fonction de calcul des couts pour la production d'un materiau" $ do
       it "renvoie une erreur de materiau non disponible si le materiau cible est en Nothing)" $ do
         shouldBe (calculerCout params ([],Nothing,Nothing)) (Left ErrMateriau)
       it "renvoie une erreur de calcul de grande Noria s'il n'y a pas de liste d'ingredient" $ do
         shouldBe (calculerCout params ([],Just materiauOK,calculData grandeNoria' (Tonne 5))) (Left ErrCalculPNoria)
       it "renvoie une erreur de calcul de grande Noria si le calcul des trajets de grande Noria est en nothing" $ do
         shouldBe (calculerCout params (fst $ petiteNoria carteGi1Gi2PlSansGis (Tonne 5) ([ingredientOK,ingredientOK01],Just materiauOK),Just materiauOK,Nothing)) (Left ErrCalculGdNoria)

     
     
  

