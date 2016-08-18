{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ScreenOneLogic where

import qualified Control.Monad      as Mo
import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Control.Applicative as A
import           Math.Combinat.Sets (choose)

-- | Data structure

   -- | Ecran 1

data Carac = Nom
  | Ph
  | Granu4
  | Granu50
  | MacroPo
  | Mo
  | Azote
  | Polsen
  | Cec
  | ReserveU
  | Densite
  | Calcite deriving (Eq,Ord,Show)

type Proportion = Double

data CaracValeur = Id String | Val Double deriving (Eq,Show)

data Agreg = Append CaracValeur Proportion
            | Arithmetic CaracValeur Proportion
            | Geometric CaracValeur Proportion
            | Harmonic CaracValeur Proportion
            | Quadratic CaracValeur Proportion deriving (Eq,Show)


class Sol a where
  nom :: a -> Maybe String
  valeur :: Carac -> a -> Maybe CaracValeur
  enTonne :: Quantite -> a -> Maybe Quantite
  enVolume :: Quantite -> a -> Maybe Quantite

type Materiau = M.Map Carac CaracValeur

instance Sol Materiau where
  nom x = case M.lookup Nom x of
                   Just (Id x) -> Just x
                   _           -> Nothing
  valeur c x = M.lookup c x
  enTonne (Tonne x) _ = Just $ Tonne x
  enTonne (Volume x) mat = case valeur Densite mat of
                                 Just (Val y) -> Just $ Tonne (y * x)
                                 _ -> Nothing
  enVolume (Volume x) _ = Just $ Volume x
  enVolume (Tonne x) mat = case valeur Densite mat of
                             Just (Val y) -> Just $ Volume (x/y)
                             _ -> Nothing



type Ingredient = M.Map Carac Agreg

instance Sol Ingredient where
    nom ing = case M.lookup Nom ing of
                    Just (Append (Id x) _) -> Just x
                    _                      -> Nothing
    valeur c x = case M.lookup c x of
                    Just (Append x _) -> Just x
                    Just (Arithmetic x _) -> Just x
                    Just (Geometric x _) -> Just x
                    Just (Quadratic x _) -> Just x
                    Just (Harmonic x _) -> Just x
                    _ -> Nothing
    enTonne (Tonne x) _ = Just $ Tonne x
    enTonne (Volume x) ing = case valeur Densite ing of
                                Just (Val y) -> Just $ Tonne (x * y)
                                _ -> Nothing
    enVolume (Volume x) _ = Just $ Volume x
    enVolume (Tonne x) ing = case valeur Densite ing of
                                Just (Val y) -> Just $ Volume (x/y)
                                _ -> Nothing

type Recette = M.Map Carac (CaracValeur -> Proportion -> Agreg)


   -- | Ecran 2

data Quantite = Volume Double | Tonne Double deriving (Show)

class Vec a where
  addVec :: a -> a -> Maybe a
  multScal :: Double -> a -> a
  neg :: a -> a


instance Vec Quantite where
  addVec (Volume x) (Volume y) = Just $ Volume (x+y)
  addVec (Tonne x) (Tonne y) = Just $ Tonne (x + y)
  addVec _ _ = Nothing
  multScal x (Volume y) = Volume (x*y)
  multScal x (Tonne y) = Tonne (x*y)
  neg (Tonne x) = Tonne (-x)
  neg (Volume x) = Volume (-x)


instance Eq Quantite where
  (==) (Volume x) (Volume y) = x == y
  (==) (Tonne x) (Tonne y) = x == y
  (==) _ _ = False

instance Ord Quantite where
  compare (Volume x) (Volume y) = compare x y
  compare (Tonne x) (Tonne y) = compare x y
  compare _ _ = error "Volume et tonnage ne peuvent etre compares"


-- | Fonctions

   -- | Ecran 1

   --   ecran1 :: ([([Ingredient], materiau)],[materiau])

-- Produit un ingredient pret a etre utilisé pour un mélange
-- à partir d'une recette, d'un materiau et d'une proportion
mkIngredient :: Recette -> Materiau -> Proportion -> Ingredient
mkIngredient r m p = fmap ($ p) matRecette
                        where matRecette = M.intersectionWith ($) r m

-- Produit un matériau a partir d'une liste d'ingrédients (retourne la liste
--d'ingrédient utilisés en fst)
melanger :: [Ingredient] -> ([Ingredient], Maybe Materiau)
melanger [] = ([],Nothing)
melanger listeIng = (listeIng, reduire . ajouter $ listeIng)
                 where
                   reduire :: Maybe Ingredient -> Maybe Materiau
                   reduire xs = xs >>= (sequence . (M.map reducteur))
                   ajouter :: [Ingredient] -> Maybe Ingredient
                   ajouter xs = F.foldrM (\x acc -> sequence $ M.intersectionWith melangeur x acc)
                                         (ingredient0 xs) xs
                   ingredient0 xs = M.map zeroing (head xs)

zeroing :: Agreg -> Agreg
zeroing (Arithmetic _ _) = Arithmetic (Val 0) 0
zeroing (Geometric _ _) = Geometric (Val 0) 0
zeroing (Harmonic _ _) = Harmonic (Val 0) 0
zeroing (Quadratic _ _) = Quadratic (Val 0) 0
zeroing (Append _ _) = Append (Id "") 0

-- Définit comment sommer deux caractéristiques de deux ingrédients
melangeur :: Agreg -> Agreg -> Maybe Agreg
melangeur (Arithmetic (Val x) a) (Arithmetic (Val y) b) =
  Just $ Arithmetic (Val (x*a + y)) (a+b)
melangeur (Geometric (Val x) a) (Geometric (Val y) b) | x <= 0 = Nothing
                                                      | otherwise =
                                                        Just $ Geometric (Val (a*log x + y)) (a+b)
melangeur (Harmonic (Val x) a) (Harmonic (Val y) b) = case x of
                                            0 -> Nothing
                                            otherwise -> Just $ Harmonic (Val (a/x + y)) (a+b)

melangeur (Quadratic (Val x) a) (Quadratic (Val y) b) = Just $ Quadratic (Val (x^2 * a + y)) (a+b)
melangeur (Append (Id x) _) (Append (Id y) _) = Just $ Append (Id (" - " ++ x ++ y)) 0
melangeur _ _ = Nothing


-- Définit comment réduire après avoir mélangé
reducteur :: Agreg -> Maybe CaracValeur
reducteur (Arithmetic (Val x) a) = case a of
                               0 -> Nothing
                               otherwise -> Just $ Val (x/a)
reducteur (Geometric (Val x) a) = case a of
                               0 -> Nothing
                               otherwise -> Just $ Val (exp (x/a))
reducteur (Harmonic (Val x) a) = case x of
                               0 -> Nothing
                               otherwise -> Just $ Val (a/x)
reducteur (Quadratic (Val x) a) | x <= 0 = Nothing
                                | otherwise = Just $ Val (sqrt (x/a))

reducteur (Append (Id x) _) = Just (Id (drop 3 x))

reducteur _ = Nothing

-- Produit l'ensemble des materiaux résultants de mélanges
-- possibles à partir d'un nombre minimum et maximum d'ingredient, d'une recette
-- et d'une liste de materiaux
melangerTous :: Rational -> Int -> Int -> Recette
                 -> [(Materiau,Rational,Rational)] -> Maybe [([Ingredient], Maybe Materiau)]
melangerTous d a b r xs = (filter outNothing) <$> (map melanger) <$> allScenarios
                             where allScenarios = scenarioCreateur d a b r xs
                                   outNothing (_,Nothing) = False
                                   outNothing _ = True

-- Produit l'ensemble des mélanges possibles
-- à partir d'un nombre minimum et maximum d'ingredient, d'une recette et d'une liste de matériaux
scenarioCreateur :: Rational -> Int -> Int -> Recette
                 -> [(Materiau,Rational,Rational)] -> Maybe [[Ingredient]]
scenarioCreateur d a b r xs
  | a < 1 = Nothing
  | b > 5 = Nothing
  | length xs < b = Nothing
  | d < 0 || d > 1 = Nothing
  | otherwise = Just $ (creeMelange a b) $ appliqueRecette r xs
     where appliqueRecette :: Recette -> [(Materiau,Rational,Rational)] ->
                                  [((Proportion->Ingredient),Rational,Rational)]
           appliqueRecette r' xs' = map (\(mat,deb,fin) -> ((mkIngredient r' mat),deb,fin)) xs'
           creeMelange :: Int -> Int -> [((Proportion->Ingredient),Rational,Rational)] -> [[Ingredient]]
           creeMelange a b ys = (creeProportion d) combinaisons
                                 where combinaisons = concatMap (flip choose $ ys) [a..b]

-- Produit l'ensemble des mélanges d'ingrédients possibles par pas de 'd'  à partir de
-- l' ensemble de combinaisons d'ingrédients possibles
creeProportion :: Rational ->
                  [[((Proportion -> Ingredient),Rational,Rational)]] ->
                  [[Ingredient]]
creeProportion d xs = concatMap prop xs
                      where prop :: [((Proportion->Ingredient),Rational,Rational)] ->
                                    [[Ingredient]]
                            prop [] = [[]]
                            prop xs = conc $ filter propEqual1 $ sequence $ map creerDeclinaison xs
                                       where creerDeclinaison :: ((Proportion -> Ingredient), Rational, Rational) ->
                                                                 [((Proportion -> Ingredient),Rational)]
                                             creerDeclinaison (fun,r1,r2) = [(fun,x) | x <- [r1,(r1+d)..r2]]
                                             propEqual1 :: [((Proportion -> Ingredient),Rational)] -> Bool
                                             propEqual1 xs = (foldr (\(fun,prop) acc -> prop + acc)
                                                              (0 :: Rational) xs) == (1 :: Rational)
                                             conc = (fmap.fmap) (\(fun,prop) -> fun $ fromRational prop)



