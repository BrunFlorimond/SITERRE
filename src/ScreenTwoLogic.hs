module ScreenTwoLogic where

import ScreenOneLogic
import qualified Data.Map           as M
import qualified Control.Monad      as Mo
import qualified Data.List          as L
import           Data.Maybe (fromJust)

type Localisation = String


data Plateforme = Plateforme PrixMelange PrixStocke PrixAnalyse Camion Materiau | PlateformeSMat PrixMelange PrixStocke PrixAnalyse Camion deriving (Eq,Show)

type PrixMelange = Double
type PrixStocke = Double
type PrixAnalyse = Double



data Camion = Camion
     {  conso           :: Double
      , emissionsCO2    :: Double
      , tonnageMax      :: Double
      , volumeMax       :: Double
      , prixLocation    :: Double
     }  deriving (Eq,Show)

data Gisement = Gisement
     {  gMateriau :: Materiau
      , gQuantite :: Quantite
      , gCamion   :: Camion
      , gPrix     :: Double
     } deriving (Eq,Show)

instance Sol Gisement where
  nom gis         =  nom (gMateriau gis)
  valeur c gis    =  valeur c (gMateriau gis)
  enTonne x gis   =  enTonne x (gMateriau gis)
  enVolume x gis  =  enVolume x (gMateriau gis)

data Chantier = Chantier deriving (Eq,Show)
     
data Site = Site
     { loc         :: Localisation
      ,typesite    :: TypeSite
     } deriving (Eq,Show)

data TypeSite = Gi Gisement | Pl Plateforme | Ch Chantier deriving (Eq,Show)

type Depart = Site
type Arrivee = Site
data Distance = Distance
     {  km            ::     Double
      , temps         ::     Double
     }  deriving(Eq,Show)


data Trajet = Trajet
     {  depart        ::     Depart
      , distance      ::     Distance
     } deriving (Eq,Show)

type NomSite = String

data Carte = Carte PetiteNoria GrandeNoria deriving (Eq,Show)

type PetiteNoria = [Trajet]
type GrandeNoria = Trajet

data DataTrajet = DataTrajet
      { trajet          ::     Trajet
      , location        ::     Duree
      , carburant       ::     QtyCarburant
      , dataSite        ::     DataSite
     } deriving (Eq,Show)

     
data DataSite = DataSite Quantite | DataPf Melange Stockage Analyse | DataCh Incorp deriving (Eq,Show)

type Achat = Double
type Melange = Double
type Stockage = Double
type Analyse = Double
type Incorp =  Double

data TrajetCout = TrajetCout
     {  tousTrajets    ::     [Trajet]
      , clocation      ::     Double
      , ccarburant     ::     Double
      , cCO2           ::     Double
      , eCO2           ::     Double
      , cachat         ::     Double
      , cMelange       ::     Double
      , cStockage      ::     Double
      , cAnalyse       ::     Double
      , dureeLocation  ::     Double
      , litreCarburant   ::     Double
     }  deriving (Eq,Show)

data Couts = Couts [(Ingredient,[TrajetCout])] TrajetCout Materiau deriving (Eq,Show)

data ErrorCouts = ErrCalculPNoria | ErrCalculIngredient | ErrCalculGdNoria | ErrParams | ErrMateriau deriving (Eq,Show)

data InfoTrajet = InfoTrajet DataTrajet TrajetCout deriving (Eq,Show)

type Duree = Double
type QtyCarburant = Double

data Params = Params
  {  prixCO2             :: Double
   , prixCarburant       :: Double
  } deriving (Eq,Show)

dureeJournee = 60*60*24
dureeDemiJournee = dureeJournee / 2
emissionsParLitre = 2.6


calculer :: [([Ingredient], Maybe Materiau)] -> Carte -> Quantite -> Params -> [Either ErrorCouts Couts]
calculer xs carte quantite params = calculerCouts params $ produireTrajets xs quantite carte
 

calculerCouts :: Params -> [([(Ingredient,Maybe [DataTrajet])],Maybe Materiau,Maybe DataTrajet)] -> [Either ErrorCouts Couts]
calculerCouts p xs = map (calculerCout p) xs

-- Calcul les couts à partir des données de trajets (conso essence, temps etc...)
calculerCout :: Params -> ([(Ingredient,Maybe [DataTrajet])],Maybe Materiau,Maybe DataTrajet) -> Either ErrorCouts Couts
calculerCout _ (_,Nothing,_) = Left ErrMateriau
calculerCout _ ([],_,_) = Left ErrCalculPNoria
calculerCout _(_,_,Nothing) = Left ErrCalculGdNoria
calculercout params (li,mat,gNoria) = case sequence $ map snd li of
                           Nothing -> Left ErrCalculIngredient
                           otherwise -> Right $ Couts (calcCoutPNoria params li) (calcCoutNoria params (fromJust gNoria)) (fromJust mat)

calcCoutPNoria :: Params -> [(Ingredient, Maybe [DataTrajet])] -> [(Ingredient,[TrajetCout])]
calcCoutPNoria params li = map calcCoutNoria' li
    where calcCoutNoria' (ing, dataTraj) = (ing, map (calcCoutNoria params) (fromJust dataTraj) )

calcCoutNoria :: Params -> DataTrajet -> TrajetCout
calcCoutNoria params dataTrajet  = TrajetCout [trajet dataTrajet] coutLocation coutCarburant coutCO2 emissionsCO2 coutAchat coutMelange coutStockage coutAnalyse (location dataTrajet) (carburant dataTrajet)
   where coutLocation = prixDemiJournee  *  fromIntegral (ceiling nbDemiJoursNecessaires)
         prixDemiJournee = case typesite $ depart $ trajet dataTrajet of
           (Gi gisement) -> (prixLocation $ gCamion gisement) / 2
           (Pl (Plateforme _ _ _ camion _)) -> (prixLocation camion) / 2
           (Pl (PlateformeSMat _ _ _ camion)) -> (prixLocation camion) / 2
           otherwise -> 0
         nbDemiJoursNecessaires = (location dataTrajet) / dureeDemiJournee
         coutCarburant = (prixCarburant params) * (carburant dataTrajet)
         emissionsCO2 = (carburant dataTrajet) * emissionsParLitre
         coutCO2 = (emissionsCO2 / 1000) * (prixCO2 params)
         coutMelange = case dataSite dataTrajet of
                         (DataSite _) -> 0
                         (DataPf melange _ _) -> melange
         coutStockage = case dataSite dataTrajet of
                         (DataSite _) -> 0
                         (DataPf _ stockage _) -> stockage
         coutAnalyse = case dataSite dataTrajet of
                         (DataSite _) -> 0
                         (DataPf _ analyse _) -> analyse
         coutAchat = case dataSite dataTrajet of
           (DataSite x) -> case typesite $ depart $ trajet $ dataTrajet of
                            (Gi gisement) -> case x of
                              (Tonne tonne)   -> tonne * (gPrix gisement)
                              (Volume volume) -> case enTonne (Volume volume) (gMateriau gisement) of
                                (Just (Tonne t)) -> t * (gPrix gisement)
                                otherwise        -> 0
                            otherwise     -> 0

           otherwise   -> 0

-- Produit les trajets de la petite et grande noria
produireTrajets :: [([Ingredient],Maybe Materiau)] -> Quantite -> Carte  -> [([(Ingredient,Maybe [DataTrajet])],Maybe Materiau,Maybe DataTrajet)]
produireTrajets ec1 q tra = map (grandeNoria tra q) $ (map (petiteNoria tra q))  (filter (verifierPresence tra) ec1)

-- Verifie que chaque Ingredient dispose d'au moins un gisement disposant du meme materiau
verifierPresence :: Carte -> ([Ingredient],Maybe Materiau) ->  Bool
verifierPresence _ ([],_) = False
verifierPresence _ (_,Nothing) = False
verifierPresence (Carte trajets _) (ing,_) = all (checkPres trajets) ing
                                  where checkPres :: PetiteNoria -> Ingredient -> Bool
                                        checkPres listTrajets ing = any (gisementDe ing) listTrajets
                                        gisementDe :: Ingredient -> Trajet -> Bool
                                        gisementDe ing tra = case typesite $ depart tra of
                                          Gi (Gisement mat _ _ _ ) -> case nom mat of
                                                                        Nothing -> False
                                                                        otherwise -> nom mat == nom ing
                                          otherwise -> False
-- Calcul les données de trajet de la grande noria
grandeNoria :: Carte -> Quantite -> ([(Ingredient,Maybe [DataTrajet])],Maybe Materiau) -> ([(Ingredient,Maybe [DataTrajet])],Maybe Materiau,Maybe DataTrajet)
grandeNoria _ _ (_,Nothing) = ([],Nothing,Nothing)
grandeNoria (Carte _ trajet) (Volume x) (li,mat) | x <= 0 = (li,mat,Nothing)
                                                 | otherwise = (li,mat,calculData (appliquerMatPlateforme trajet mat) (Volume x))
grandeNoria (Carte _ trajet) (Tonne x) (li,mat)  | x <= 0 =(li,mat,Nothing)
                                                 | otherwise = (li,mat,calculData (appliquerMatPlateforme trajet mat) (Tonne x))

appliquerMatPlateforme :: Trajet -> Maybe Materiau -> Trajet
appliquerMatPlateforme (Trajet (Site loc ts) distance) mat = case mat of
 (Just materiau) -> (Trajet (Site loc (creerPlateforme ts) ) distance)
   where creerPlateforme tpsite = case tpsite of
          (Pl (Plateforme a b c d e)) -> Pl (Plateforme a b c d e)
          (Pl (PlateformeSMat mel sto an cam)) -> (Pl (Plateforme mel sto an cam materiau))
          otherwise -> tpsite
 otherwise -> (Trajet (Site loc ts) distance)                                                   


-- Assigne une quantité de materiau a extraire pour chaque Gisement en prenant le gisement le plus proche en premier et calculs les données du trajet (duree, consommation d'essence...)
petiteNoria :: Carte -> Quantite -> ([Ingredient],Maybe Materiau) -> ([(Ingredient,Maybe [DataTrajet])],Maybe Materiau)
petiteNoria _ _ (_,Nothing) = ([],Nothing)
petiteNoria carte quantite (ingredients,mat) = (map dispatcherQuantite ingredients, mat)
                                   where dispatcherQuantite :: Ingredient -> (Ingredient,Maybe [DataTrajet])
                                         dispatcherQuantite ing = (ing, (dispatch (quantiteNecessaire ing) (filtrerClasserGisement ing carte)))
                                         filtrerClasserGisement :: Ingredient -> Carte  -> [Trajet]
                                         filtrerClasserGisement ing (Carte petiteNoria _) = L.sortOn (\(Trajet _ (Distance l _)) -> l) (filter (materiauNecessaire ing) petiteNoria)
                                         materiauNecessaire :: Ingredient -> Trajet -> Bool
                                         materiauNecessaire ing' (Trajet (Site _ typeSite) _) = case typeSite of
                                               (Gi gisement) -> nom ing' == nom gisement
                                               otherwise     -> False
                                         quantiteNecessaire :: Ingredient -> Maybe Quantite
                                         quantiteNecessaire ing' = multScal <$> (proportion ing') <*> (enTonne quantite ing')
                                        
                                               

-- Produit la proportion d'un ingredient
proportion :: Ingredient -> Maybe Double
proportion ing = case M.lookup Nom ing of
                     Just (Append _ prop) -> Just prop
                     _ -> Nothing

-- Effectue le dispatch Quantite-Trajet et produit les données de chaque trajet
dispatch :: Maybe Quantite -> [Trajet] -> Maybe [DataTrajet]
dispatch _ [] = Nothing
dispatch Nothing _ = Nothing
dispatch quantite trajets = case quantite of
                   Just (Volume _) -> Nothing
                   Just (Tonne x) -> case compare x 0 of
                                      GT -> dispatch' trajets [] (Just (Tonne 0)) quantite
                                      _ -> Nothing

dispatch' :: [Trajet] -> [Maybe DataTrajet] -> Maybe Quantite -> Maybe Quantite -> Maybe [DataTrajet] 
dispatch' _ _ Nothing _ = Nothing
dispatch' [] res acc q  | acc >=  q = sequence $ reverse res
                        | otherwise = Nothing
dispatch' trajets res acc q | acc >=  q = sequence $ reverse res
                            | otherwise = dispatch' (tail trajets) (((calculData (head trajets)) =<< qty') : res) (Mo.join $ addVec <$> qty' <*> acc) q
                                       where qty :: Trajet  -> Maybe Quantite
                                             qty (Trajet (Site _ typeSite) _) =
                                               case typeSite of
                                                 (Gi gisement) ->
                                                   case Mo.join $ safeCompare <$> restantNecessaire <*> (enTonne (gQuantite gisement) gisement) of
                                                                                Just LT -> restantNecessaire
                                                                                Just EQ -> enTonne (gQuantite gisement) gisement
                                                                                Just GT -> enTonne (gQuantite gisement) gisement
                                                                                _       -> Nothing
                                                 otherwise     -> Nothing
                                             qty' = qty (head trajets)
                                             restantNecessaire :: Maybe Quantite
                                             restantNecessaire = Mo.join $ addVec <$> q <*> (neg <$> acc)


safeCompare :: Quantite -> Quantite -> Maybe Ordering
safeCompare (Volume x) (Volume y) = Just $ compare x y
safeCompare (Tonne x) (Tonne y) = Just $ compare x y
safeCompare _ _= Nothing

-- Calcul les données de chaque trajet
calculData :: Trajet -> Quantite -> Maybe DataTrajet
calculData trajet quantite =
  case typesite $ depart trajet of
   (Gi (Gisement materiau _ camion prix)) -> Just $
     DataTrajet  trajet duree carburant (DataSite $ quantite)
      where duree = (calculerDuree quantite camion materiau tempsTraj)
            carburant = (calculerCarburant quantite camion materiau kmTraj)
   (Pl (Plateforme pMelange pStock pAnalyse camion materiau)) -> Just $
    (DataTrajet trajet duree carburant (DataPf (calculerMSA' pMelange) (calculerMSA' pStock) (calculerMSA' pAnalyse)))
      where duree = (calculerDuree quantite camion materiau tempsTraj)
            carburant = (calculerCarburant quantite camion materiau kmTraj)
            calculerMSA' = calculerMSA quantite materiau
   otherwise -> Nothing
  where tempsTraj = temps $ distance trajet
        kmTraj    = km $ distance trajet

-- Calcule la durée totale de la noria
calculerDuree :: Quantite -> Camion -> Materiau -> Double -> Double
calculerDuree quantite camion materiau temps = fromIntegral (nbAllerRetours quantite camion materiau)  * temps * 2

-- Calcule le nombre d'aller retour en fonction de la quantité necessaire et du tonnage et du volume max du camion
nbAllerRetours :: Quantite -> Camion -> Materiau -> Int
nbAllerRetours quantite camion materiau =
  case quantite of
    (Volume volume) -> case allerRetourTonnage of
                           (Just (Tonne x)) -> max (ceiling x) allerRetourVolume
                           otherwise        -> allerRetourVolume
                          where allerRetourTonnage = multScal (1/(tonnageMax camion)) <$> (enTonne quantite materiau)
                                allerRetourVolume  = (ceiling (volume / (volumeMax camion))) 
    (Tonne tonne) -> case allerRetourVolume of
                           (Just (Volume x)) -> max (ceiling x) allerRetourTonnage
                           otherwise         -> allerRetourTonnage
                          where allerRetourVolume = multScal (1/(volumeMax camion)) <$> (enVolume quantite materiau)
                                allerRetourTonnage = (ceiling (tonne / (tonnageMax camion)))
  

-- Calcule la quantité de carburant necessaire pour effectuer la noria
calculerCarburant :: Quantite -> Camion -> Materiau -> Double -> Double
calculerCarburant quantite camion materiau km = ((fromIntegral (nbAllerRetours quantite camion materiau) * km * 2) / 100 ) * (conso camion)

-- Calcule les coûts linéaires en fonction de la quantité
calculerMSA :: Quantite -> Materiau -> Double -> Double
calculerMSA (Tonne tonnage) _ prix = tonnage * prix
calculerMSA volume materiau prix = case enTonne volume materiau of
                          Just (Tonne x) -> x * prix
                          otherwise -> 0 


