module ScreenOneLogicTest where

import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           ScreenOneLogic
import           Test.Hspec
import           DataTest



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
  

