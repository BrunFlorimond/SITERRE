module Test where

import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           Main
import           Test.Hspec

emptyMateriau = (M.empty :: M.Map Carac CaracValeur)
emptyIngredient = (M.empty :: M.Map Carac Agreg)
emptyRecette = (M.empty :: M.Map Carac (CaracValeur -> Proportion -> Agreg))
recette = M.fromList [(Nom, Append), (Granu4, Arithmetic),(Granu50, Geometric),(MacroPo, Harmonic),(Mo, Arithmetic),(Azote, Geometric),(Polsen, Harmonic),(Cec, Quadratic),(ReserveU,Quadratic)]

materiau1 = M.fromList [(Nom, Id "Mat1"), (Granu4, Val 0),(Granu50, Val 1),(MacroPo, Val (-1)),(Mo, Val 58.245654),(Azote, Val (-877585.5454)),(Polsen, Val 456),(Cec,Val 5),(ReserveU,Val 5)]

materiau2 = M.fromList [(Nom, Id "Mat2"), (Granu4, Val 5),(Granu50, Val 6),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9)]

materiau3 = M.fromList [(Nom, Id "Mat3") , (Granu4, Val 0),(Granu50, Val 1),(MacroPo, Val (1)),(Mo, Val 58.245654),(Azote, Val (877585.5454)),(Polsen, Val 456),(Cec,Val 5)]

materiau4 = M.fromList [(Nom, Id "Mat4"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9)]

materiau5 = M.fromList [(Nom, Id "Mat4"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9),(Densite, Val 1.5)]

materiauSansNom = M.fromList [(Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9)]

ingredient1' = M.fromList [(Granu4, Arithmetic (Val 0) 1),(Granu50, Geometric (Val 1) 1),(MacroPo, Harmonic (Val (-1)) 1),(Mo, Arithmetic (Val 58.245654) 1),(Azote, Geometric (Val (-877585.5454)) 1),(Polsen, Harmonic (Val 456) 1),(Cec,Quadratic (Val 5) 1),(ReserveU,Quadratic (Val 5) 1)]

ingredient1 = M.fromList [(Nom, Append (Id "Mat1") 0.5),(Granu4, Arithmetic (Val 0) 0.5),(Granu50, Geometric (Val 1) 0.5),(MacroPo, Harmonic (Val (-1)) 0.5),(Mo, Arithmetic (Val 58.245654) 0.5),(Azote, Geometric (Val (-877585.5454)) 0.5),(Polsen, Harmonic (Val 456) 0.5),(Cec,Quadratic (Val 5) 0.5),(ReserveU,Quadratic (Val 5) 0.5)]

ingredientGeom0 = M.fromList [(Granu4, Arithmetic (Val 0) 0.5),(Granu50, Geometric (Val 0) 0.5),(MacroPo, Harmonic (Val (-1)) 0.5),(Mo, Arithmetic (Val 58.245654) 0.5),(Azote, Geometric (Val (-877585.5454)) 0.5),(Polsen, Harmonic (Val 456) 0.5)]

ingredientHarm0 = M.fromList [(Granu4, Arithmetic (Val 0) 0.5),(Granu50, Geometric (Val 1) 0.5),(MacroPo, Harmonic (Val (-1)) 0.5), (Mo, Arithmetic (Val 58.245654) 0.5),(Azote, Geometric (Val (-877585.5454)) 0.5),(Polsen, Harmonic (Val 0) 0.5)]

ingredientAri0 = M.fromList [(Granu4, Arithmetic (Val 1) 0.5),(Granu50, Geometric (Val 1) 0.5),(MacroPo, Harmonic (Val (-1)) 0.5),(Mo, Arithmetic (Val 58.245654) 0.5),(Azote, Geometric (Val 1)  0.5),(Polsen, Harmonic (Val 1) 0.5)]

ingredientAri01 = M.fromList [(Granu4, Arithmetic (Val 0) (-0.5)),(Granu50, Geometric (Val 1) 0.5),(MacroPo, Harmonic (Val 1) 0.5),(Mo, Arithmetic (Val 58.245654) 0.5),(Azote, Geometric (Val 1) 0.5),(Polsen, Harmonic (Val 1) 0.5)]

ingredientOK = M.fromList [(Nom, Append (Id "Mat1") 0.5), (Granu4, Arithmetic (Val 10) 0.5),(Granu50, Geometric (Val 10) 0.5),(MacroPo, Harmonic (Val 10) 0.5),(Mo, Arithmetic (Val 20) 0.5),(Azote, Geometric (Val 20) 0.5),(Polsen, Harmonic (Val 20) 0.5),(Cec, Quadratic (Val 10) 0.5), (ReserveU, Quadratic (Val 20) 0.5)]

ingredientOK01 = M.fromList [(Nom, Append (Id "Mat2") 0.5),(Granu4, Arithmetic (Val 20) 0.5),(Granu50, Geometric (Val 20) 0.5),(MacroPo, Harmonic (Val 20) 0.5),(Mo, Arithmetic (Val 10) 0.5),(Azote, Geometric (Val 10) 0.5),(Polsen, Harmonic (Val 10) 0.5),(Cec,Quadratic (Val 20) 0.5),(ReserveU,Quadratic (Val 10) 0.5)]

ingredientOK02 = M.fromList [(Nom, Append (Id "Mat2") (1/5)), (Granu4, Arithmetic (Val 10) (1/5)),(Granu50, Geometric (Val 10) (1/5)),(MacroPo, Harmonic (Val 10) (1/5)),(Mo, Arithmetic (Val 10) (1/5)),(Azote, Geometric (Val 10) (1/5)),(Polsen, Harmonic (Val 10) (1/5)),(Cec, Quadratic (Val 10) (1/5)), (ReserveU, Quadratic (Val 10) (1/5))]

ingredientOK03 = M.fromList [(Nom, Append (Id "Mat3") (2/5)),(Granu4, Arithmetic (Val 20) (2/5)),(Granu50, Geometric (Val 20) (2/5)),(MacroPo, Harmonic (Val 20) (2/5)),(Mo, Arithmetic (Val 30) (2/5)),(Azote, Geometric (Val 30) (2/5)),(Polsen, Harmonic (Val 30) (2/5)),(Cec,Quadratic (Val 20) (2/5)),(ReserveU,Quadratic (Val 30) (2/5))]

ingredientOK04 = M.fromList [(Nom, Append (Id "Mat4") (2/5)),(Granu4, Arithmetic (Val 30) (2/5)),(Granu50, Geometric (Val 30) (2/5)),(MacroPo, Harmonic (Val 30) (2/5)),(Mo, Arithmetic (Val 20) (2/5)),(Azote, Geometric (Val 20) (2/5)),(Polsen, Harmonic (Val 20) (2/5)),(Cec,Quadratic (Val 30) (2/5)) ,(ReserveU,Quadratic (Val 20) (2/5))]

materiauOK = M.fromList [(Nom, Id "Mat1 - Mat2"),(Granu4, (Val ((10+20)/2))),(Granu50, (Val $ exp (0.5 * log 10 + 0.5 * log 20) / 1)),(MacroPo, (Val (1/((0.5/10)+(0.5/20))))),(Mo, (Val ((10+20)/2))),(Azote, (Val $ exp (0.5 * log 10 + 0.5 * log 20) / 1)),(Polsen, (Val $ 1/((0.5/10)+(0.5/20)))),(Cec,(Val $ sqrt $ (10^2+20^2)/2)),(ReserveU, (Val $ sqrt $ (20^2+10^2)/2))]

materiauOK01 = M.fromList [(Nom, Id "Mat2 - Mat3 - Mat4"),(Granu4, (Val $ 10*(1/5)+20*(2/5)+30*(2/5))),(Granu50, (Val $ exp (((1/5) * log 10 + (2/5) * log 20 + (2/5) * log 30)/(1/5+2/5+2/5)))),(MacroPo, (Val $ 1/(((1/5)/10)+((2/5)/20)+((2/5)/30)))),(Mo, (Val $ 10*(1/5)+20*(2/5)+30*(2/5))),(Azote, (Val $ exp ((1/5) * log 10 + (2/5) * log 20 + (2/5) * log 30))),(Polsen, (Val $ 1/(((1/5)/10)+((2/5)/20)+((2/5)/30)))),(Cec,(Val $ sqrt $ (10^2) * (1/5)+(20^2) * (2/5) + (30^2)* (2/5))),(ReserveU,(Val$  sqrt $ (10^2) * (1/5)+(20^2) * (2/5) + (30^2)* (2/5)))]


qt0 = Tonne 0
qt1 = Tonne 5
qt2 = Tonne 55.269
qt3 = Tonne 75.89

qtV0 = Volume 0
qtV1 = Volume 5
qtV2 = Volume 55.269
qtV3 = Volume 75.89

loc = Localisation "Grenoble" 0
loc1 = Localisation "Grenoble" 0
loc2 = Localisation "Grenoble" 5
loc3 = Localisation "Grenoble" 12.588
loc4 = Localisation "Grenoble" 47.9998

gisement1 = Gisement materiau1 loc1 qt1
gisement2 = Gisement materiau2 loc2 qt1
gisement3 = Gisement materiau3 loc3 qt1
gisement4 = Gisement materiau4 loc4 qt1
gisement5 = Gisement materiau5 loc4 qt2
gisementSansNom = Gisement materiauSansNom loc qt1



wannabeIngredient1 = mkIngredient recette materiau1
wannabeIngredient2 = mkIngredient recette materiau2
wannabeIngredient3 = mkIngredient recette materiau3
wannabeIngredient4 = mkIngredient recette materiau4

listeMelange = melangerTous 0.05 1 4 recette [(materiau1,0,1), (materiau2,0,1), (materiau3,0,1), (materiau4,0,1)]

main :: IO ()
main = hspec $ do
  describe "test de mkIngredient (creation d'un ingredient)" $ do
           it "retourne un ingredient vide si le materiau est vide et la recette est vide" $ do
             shouldBe (mkIngredient emptyRecette emptyMateriau 0) emptyIngredient
           it "retourne un ingredient vide si le materiau est vide" $ do
             shouldBe (mkIngredient recette emptyMateriau 0) emptyIngredient
           it "retourne un ingredient vide si la recette est vide" $ do
             shouldBe (mkIngredient emptyRecette materiau1 0) emptyIngredient
           it "retourne un ingredient avec sa proportion" $ do
             shouldBe (mkIngredient recette materiau1 0.5) ingredient1
  describe "test de melanger avec 2 ingredients" $ do
           it "retourne Nothing si la liste des ingredients a melanger est vide" $ do
             shouldBe (melanger []) ([], Nothing)
           it "retourne Nothing si une caracteristique agregée par Geometrie est à 0" $ do
             shouldBe (melanger [ingredientGeom0,ingredientGeom0]) ([ingredientGeom0,ingredientGeom0] ,Nothing)
           it "retourne Nothing si une caracteristique agregee par Harmonie est à 0" $ do
             shouldBe (melanger [ingredientHarm0,ingredientHarm0]) ([ingredientHarm0,ingredientHarm0],Nothing)
           it "retourne Nothing si la moyenne pondérée entraine une division par 0" $ do
             shouldBe (melanger [ingredientAri0,ingredientAri01]) ([ingredientAri0,ingredientAri01],Nothing)
           it "retourne l'agregation juste des deux ingredients" $ do
             shouldBe (melanger [ingredientOK,ingredientOK01]) ([ingredientOK,ingredientOK01],Just materiauOK)
--  describe "test de melanger avec 3 ingredients" $ do
--           it "retourne l'agregation des 3 ingrédients" $ do
--             shouldBe (melanger [ingredientOK02, ingredientOK03, ingredientOK04]) (Just materiauOK01)
--           it "retourne une liste de tous les mélanges possibles de trois ingrédients v2" $ do
--             shouldBe (creeProportion (0.05) [[(wannabeIngredient1,0.1,0.9),(wannabeIngredient2,0.9,1)],[(wannabeIngredient1,0.1,0.9), (wannabeIngredient3,0.95,1)],[(wannabeIngredient2,0.5,0.5),(wannabeIngredient3,0.5,0.5)]]) [[]]
          -- it "retourne une liste de tous les mélanges possibles de trois ingrédients" $ do
           --  shouldBe (creeProportion (0.01) [[(wannabeIngredient1,0,0.3), (wannabeIngredient2,0.3,0.3),(wannabeIngredient3,0.4,1)]]) []
  describe "test de creation de scenarios" $ do
           it "retourne nothing si le nombre maxi est supérieur au nombre de materiau" $ do
             shouldBe (scenarioCreateur (0.05) 2 5 recette [(materiau1,0,1),(materiau2,0,1),(materiau3,0,1),(materiau4,0,1)]) Nothing
           it "retourne les ingredients a 1 si le nombre mini et maxi sont 1" $ do
             shouldBe (scenarioCreateur 0.05  1 1 recette [(materiau1,0,1), (materiau2,0,1), (materiau3,0,1), (materiau4,0,1)]) (Just $ [[wannabeIngredient1 1],[wannabeIngredient2 1], [wannabeIngredient3 1], [wannabeIngredient4 1]])

  describe "test de Verifications de présences de materiau-mélange / materiau-gisement" $ do
           it "retourne Nothing s'il n'y a pas de gisement" $ do
             shouldBe (verifierPresence [] (fromJust (head <$> listeMelange))) Nothing
           it "retourne Nothing s'il n'y as pas de matériau résultat" $ do
             shouldBe (verifierPresence [gisement1,gisement2,gisement3,gisement4] ([ingredientOK01,ingredientOK02],Nothing)) Nothing
           it "retourne Nothing s'il n'y a pas de materiau à mélanger" $ do
             shouldBe (verifierPresence [gisement1,gisement2,gisement3,gisement4] ([],Just materiau1)) Nothing
           it "retourne Nothing si l'un des ingredients n'a pas de champs Nom" $ do
             shouldBe (verifierPresence [gisement1,gisement2,gisement3,gisement4] ([ingredientOK01,ingredient1',ingredientOK03],Just materiau1)) Nothing
           it "retourne Nothing si l'un des gisements n'a pas de champs Nom" $ do
             shouldBe (verifierPresence [gisement1,gisement2,gisement3,gisement4,gisementSansNom] ([ingredientOK01,ingredientOK02],Just materiau1)) Nothing
           it "retourne Just True si chaque ingredient a son gisement" $ do
             shouldBe (verifierPresence [gisement1,gisement2,gisement3,gisement4] ([ingredientOK,ingredientOK02],Just materiau1)) (Just True)
           it "retourne Just False car un gisement manque" $ do
             shouldBe (verifierPresence [gisement1,gisement3,gisement4] ([ingredientOK,ingredientOK02],Just materiau1)) (Just False)
  describe "test de la fonction d'assignation de quantité à chaque gisement" $ do
    it "retourne [] si la liste de gisement est vide" $ do
      shouldBe (dispatch (Just $ Tonne 30) []) []
    it "retourne [] si la quantite necessaire est  égale a 0" $ do
      shouldBe (dispatch (Just $ Tonne 0) [gisement5,gisement5,gisement5]) []
    it "retourne [] si la quantite necessaire est négative" $ do
      shouldBe (dispatch (Just $ Tonne (-5)) [gisement5,gisement5,gisement5]) []
    it "retourne [] si la quantite necessaire est exprimee en Volume" $ do
      shouldBe (dispatch (Just $ Volume 30) [gisement5,gisement5,gisement5]) []
    it "retourne [] s'il n'y a pas assez de quantite dans les gisements" $ do
      shouldBe (dispatch (Just $ Tonne 555.5) [gisement1,gisement2,gisement3]) []
    it "retourne le bon resultat avec un seul gisement suffisant (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 25.59) [Gisement materiau1 loc (Tonne 86.25879)]) [(Gisement materiau1 loc (Tonne 86.25879),Just (Tonne 25.59))]
    it "retourne le bon resultat avec deux gisements suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 25.59) [Gisement materiau1 loc (Tonne 15.789),Gisement materiau2 loc (Tonne 57.236)]) [(Gisement materiau1 loc (Tonne 15.789),Just (Tonne 15.789)),(Gisement materiau2 loc (Tonne 57.236),Just (Tonne (25.59-15.789)))]
    it "retourne le bon resultat avec trois gisements suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 25.59) [Gisement materiau1 loc (Tonne 15.789),Gisement materiau2 loc (Tonne 3.879),Gisement materiau3 loc (Tonne 25.879)]) [(Gisement materiau1 loc (Tonne 15.789),Just (Tonne 15.789)),(Gisement materiau2 loc (Tonne 3.879),Just (Tonne 3.879)),(Gisement materiau3 loc (Tonne 25.879),Just (Tonne (25.59-15.789-3.879)))]
    it "retourne le bon resultat avec trois gisements dont deux suffisants (en tonne)" $ do
      shouldBe (dispatch (Just $ Tonne 17.53) [Gisement materiau1 loc (Tonne 15.789),Gisement materiau2 loc (Tonne 3.879),Gisement materiau3 loc (Tonne 25.879)]) [(Gisement materiau1 loc (Tonne 15.789),Just (Tonne 15.789)),(Gisement materiau2 loc (Tonne 3.879),Just (Tonne (17.53-15.789)))]
    it "retourne le bon resultat avec 1 gisement en Volume" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 45)]) [(Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 45),Just $ Tonne 30)]
    it "retourne le bon resultat avec 1 gisement en Volume (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 25)]) [(Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 25),Just $ Tonne 30)]
    it "retourne [] avec 1 gisement en Volume insuffisant (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 19)]) []
    it "retourne le bon resultat avec 2 gisements en Volume (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 12),
                                    Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 9)])
        [(Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 12),Just $ Tonne (12*1.5)),
               (Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 9),Just $ Tonne (8*1.5))]
    it "retourne [] avec 2 gisements en Volume insuffisants (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 7),
                                    Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 9)])
        []
    it "retourne le bon resultat avec 2 gisements (un en volume et un en Tonne) (volume < tonne avec densite forte)" $ do
      shouldBe (dispatch (Just $ Tonne 30) [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 7),
                                    Gisement (M.fromList [(Nom,Id "mat1")]) loc (Tonne 19.5)])
        [(Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 7),Just $ Tonne (7*1.5)),
               (Gisement (M.fromList [(Nom,Id "mat1")]) loc (Tonne 19.5),Just $ Tonne 19.5)]
  describe "Pour un mélange donné assigne une quantité de matériau pour chaque ingrédient du mélange et selon les gisements disponibles" $ do
    it "retourne un tuple avec une liste vide et nothing si le materiau est en Nothing" $ do
      shouldBe (assignerQuantite [Gisement (M.fromList [(Nom,Id "mat1"),(Densite,Val 1.5)]) loc (Volume 7),Gisement (M.fromList [(Nom,Id "mat1")]) loc (Tonne 19.5)] (Tonne 30) ([],Nothing)) ([],Nothing)
    it "retourne le tuple avec une liste d'assignement gisement/quantite vide s'il n'y a pas de gisement disponible" $ do
      shouldBe (assignerQuantite [] (Tonne 30) ([ingredientOK],Just materiau1)) ([(ingredientOK,[])],Just materiau1)
    it "retourne le tuple avec une liste d'assignement gisement/quantite vide si la quantite est a 0" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 500)] (Tonne 0) ([ingredientOK],Just materiau1)) ([(ingredientOK,[])],Just materiau1)
    it "retourne le tuple avec une liste d'assignement gisement/quantite vide si la quantite est negative" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 500)] (Tonne (-50)) ([ingredientOK],Just materiau1)) ([(ingredientOK,[])],Just materiau1)
    it "retourne le tuple avec une liste d'assignement ingredient/gisement/quantite vide si la liste d'ingrédient est vide" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 500)] (Tonne (50)) ([],Just materiau1)) ([],Just materiau1)
    it "retourne le tuple juste avec un gisement pour un ingrédient" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 500)] (Tonne (50)) ([ingredientOK],Just materiau1)) ([(ingredientOK,[(Gisement materiau1 loc (Tonne 500),Just $ Tonne 25)])],Just materiau1)
    it "retourne le tuple juste avec deux gisements pour un ingrédient (avec distances différentes)" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 12),Gisement materiau1 loc2 (Tonne 14)] (Tonne 50) ([ingredientOK],Just materiau1)) ([(ingredientOK,[(Gisement materiau1 loc (Tonne 12),Just $ Tonne 12),(Gisement materiau1 loc2 (Tonne 14),Just $ Tonne 13)])],Just materiau1)
    it "retourne le tuple avec un gisement pour deux ingrédients (ne doit pas subvenir)" $ do
      shouldBe (assignerQuantite [Gisement materiau1 loc (Tonne 50)] (Tonne 50) ([ingredientOK,ingredientOK01],Just materiau1)) ([(ingredientOK,[(Gisement materiau1 loc (Tonne 50),Just $ Tonne 25)]),(ingredientOK01,[])],Just materiau1)
    it "retourne le tuple avec plusieurs gisements pour plusieurs ingrédients" $ do
      shouldBe (assignerQuantite [Gisement materiau2 loc3 (Tonne 50),Gisement materiau2 loc (Tonne 2),Gisement materiau3 loc (Tonne 50), Gisement materiau4 loc (Tonne 50)] (Tonne 50) ([ingredientOK02,ingredientOK03,ingredientOK04],Just materiauOK01)) ([(ingredientOK02,[(Gisement materiau2 loc (Tonne 2),Just $ Tonne 2),(Gisement materiau2 loc3 (Tonne 50),Just $ Tonne 8)]),(ingredientOK03,[(Gisement materiau3 loc (Tonne 50),Just $ Tonne 20)]),(ingredientOK04,[(Gisement materiau4 loc (Tonne 50),Just $ Tonne 20)])],Just materiauOK01)

