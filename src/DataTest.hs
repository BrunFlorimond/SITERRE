module DataTest where

import Data.Map as M
import ScreenOneLogic
import ScreenTwoLogic

emptyMateriau = (M.empty :: M.Map Carac CaracValeur)
emptyIngredient = (M.empty :: M.Map Carac Agreg)
emptyRecette = (M.empty :: M.Map Carac (CaracValeur -> Proportion -> Agreg))
recette = M.fromList [(Nom, Append), (Granu4, Arithmetic),(Granu50, Geometric),(MacroPo, Harmonic),(Mo, Arithmetic),(Azote, Geometric),(Polsen, Harmonic),(Cec, Quadratic),(ReserveU,Quadratic)]

materiau1 = M.fromList [(Nom, Id "Mat1"), (Granu4, Val 0),(Granu50, Val 1),(MacroPo, Val (-1)),(Mo, Val 58.245654),(Azote, Val (-877585.5454)),(Polsen, Val 456),(Cec,Val 5),(ReserveU,Val 5)]

materiau2 = M.fromList [(Nom, Id "Mat2"), (Granu4, Val 5),(Granu50, Val 6),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9)]

materiau3 = M.fromList [(Nom, Id "Mat3") , (Granu4, Val 0),(Granu50, Val 1),(MacroPo, Val (1)),(Mo, Val 58.245654),(Azote, Val (877585.5454)),(Polsen, Val 456),(Cec,Val 5)]

materiau4 = M.fromList [(Nom, Id "Mat4"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9)]

materiau5 = M.fromList [(Nom, Id "Mat4"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9),(Densite, Val 1.5)]

materiauDense = M.fromList [(Nom, Id "MatDense"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9),(Densite, Val 10)]

materiauPeuDense = M.fromList [(Nom, Id "MatPeuDense"), (Granu4, Val 5),(MacroPo, Val 7),(Mo, Val 49898.25),(Azote, Val 0.0005),(Polsen, Val 1),(Cec,Val 18),(ReserveU,Val 9),(Densite, Val 0.001)]

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
qt4 = Tonne 1000.895

qtV0 = Volume 0
qtV1 = Volume 5
qtV2 = Volume 55.269
qtV3 = Volume 75.89

locGrenoble =  "Grenoble"

camion1 = Camion 25 10 30 20 300

gisement1 = Gisement materiau1  qt1 camion1 5
gisement1' = Gisement materiau1 qt2 camion1 5
gisement2 = Gisement materiau2  qt1 camion1 6
gisement3 = Gisement materiau3  qt1 camion1 7
gisement4 = Gisement materiau4  qt1 camion1 8
gisement5 = Gisement materiau5  qt4 camion1 9
gisementMateriauDense = Gisement materiauDense qt4 camion1 12
gisementMateriauDenseV = Gisement materiauDense qtV1 camion1 12
gisementMateriauPDense = Gisement materiauPeuDense qt1 camion1 12
gisementMateriauPDenseV = Gisement materiauPeuDense qtV1 camion1 12
gisementSansNom = Gisement materiauSansNom qt1 camion1 10
gisementSansDensite = Gisement materiau1 qtV3 camion1 10

plateforme1 = Plateforme 2 2 2 camion1 materiauOK
plateformeSansGis = PlateformeSMat 2 2 2 camion1

chantier1 = Chantier 


wannabeIngredient1 = mkIngredient recette materiau1
wannabeIngredient2 = mkIngredient recette materiau2
wannabeIngredient3 = mkIngredient recette materiau3
wannabeIngredient4 = mkIngredient recette materiau4

listeMelange = melangerTous 0.05 1 4 recette [(materiau1,0,1), (materiau2,0,1), (materiau3,0,1), (materiau4,0,1)]


siteGi1 = Site locGrenoble (Gi gisement1)
siteGi1' = Site locGrenoble (Gi gisement1')
siteGi2 = Site locGrenoble (Gi gisement2)
siteGi3 = Site locGrenoble (Gi gisement3)
siteGi4 = Site locGrenoble (Gi gisement4)
siteGi5 = Site locGrenoble (Gi gisement5)

siteMateriauDense = Site locGrenoble (Gi gisementMateriauDense)
siteMateriauDenseV = Site locGrenoble (Gi gisementMateriauDenseV)
siteMateriauPDense = Site locGrenoble (Gi gisementMateriauPDense)
siteMateriauPDenseV = Site locGrenoble (Gi gisementMateriauPDenseV)
siteGiSansNom = Site locGrenoble (Gi gisementSansNom)
sitePlateforme = Site locGrenoble (Pl plateforme1)
sitePlateformeSansGis = Site locGrenoble (Pl plateformeSansGis)
siteChantier = Site locGrenoble (Ch chantier1)
siteGiSansDensite = Site locGrenoble (Gi gisementSansDensite)


trajet1 = Trajet siteGi1 (Distance 5 10)
trajet1' = Trajet siteGi1' (Distance 10 20)
trajet2 = Trajet siteGi2 (Distance 10 20)
trajet3 = Trajet siteGi3  (Distance 15 30)
trajet4 = Trajet siteGi4  (Distance 20 40)
trajet5 = Trajet siteGi5 (Distance 25 50)

trajetGiSansNom = Trajet siteGiSansNom  (Distance 25 50)
trajetMateriauDense = Trajet siteMateriauDense  (Distance 10 5)
trajetMateriauDenseV = Trajet siteMateriauDenseV  (Distance 10 5)
trajetMateriauPDense = Trajet siteMateriauPDense  (Distance 10 5)
trajetMateriauPDenseV = Trajet siteMateriauPDenseV  (Distance 10 5)
grandeNoria' = Trajet sitePlateforme  (Distance 10 20)
grandeNoriaSansGis = Trajet sitePlateformeSansGis (Distance 10 20)
trajetSansDensite = Trajet siteGiSansDensite  (Distance 10 20)

carteVide = Carte [] grandeNoria'
carteVidePlSansGis = Carte [] grandeNoriaSansGis
carteGi1 = Carte [trajet1] grandeNoria'
carteGi1Gi2 = Carte [trajet1,trajet4,trajet3,trajet2] grandeNoria'

carteGi1Gi2PlSansGis = Carte [trajet1, trajet4, trajet3, trajet2] grandeNoriaSansGis
carteIngSansNom = Carte [trajet1, trajet2, trajetGiSansNom] grandeNoria'
carteGi1Gi3 = Carte [trajet1,trajet4,trajet3] grandeNoria'

params = Params 5 1.5

dataTrajet1 = calculData trajet1 (Tonne 5)
dataTrajetGNoria = calculData grandeNoria' (Tonne 5)
