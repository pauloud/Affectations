{-# LANGUAGE RecordWildCards  #-}
module Lib(global0,Creneau,Etudiant,Global,nouvelEtudiant,nouveauCreneau,ajouterEtudiant,ajouterCreneaux,ajouterEtudiants,
    retirerCreneau,definirPrioritaires,showRangDesVoeux,voeuxCsv) where

    import qualified Data.IntMap.Strict as IntMap
    import qualified Data.HashMap.Strict as Map
    import qualified Data.List as List
    import Control.Category((<<<),(>>>))
    import Data.Maybe(fromMaybe)

    type NumEtudiant = String
    type Map = Map.HashMap
    type CleCreneau = String
    type CodeCreneau = CleCreneau
    type CleEtudiant = Int
    data Etudiant = Etudiant {numero :: NumEtudiant
                    ,voeux :: [CleCreneau]
                    ,refus :: [CleCreneau]
                    ,affectationCourante :: Maybe CleCreneau
                    ,estPrioritaire :: Bool
                    } 
    compareEt e1 e2 = if numero e1 == numero e2 then EQ else compare (length $ refus e1) (length $ refus e2)
    instance Show Etudiant where
        show Etudiant{..} = concat ["(Etudiant : ", show numero, " ", show affectationCourante, " refus : ", show refus,"\n"] 
    instance Eq Etudiant where 
        e1 == e2 = numero e1 == numero e2
    {-instance Ord Etudiant where
        compare = compareEt-}
    nouvelEtudiant :: [CodeCreneau] -> NumEtudiant-> Etudiant 
    nouvelEtudiant voeux numero = Etudiant {..} 
        where affectationCourante = Nothing
              refus = []
              estPrioritaire = False
   
    moinsInsatisfait :: IntMap.IntMap Etudiant -> CleEtudiant
    moinsInsatisfait etudiants =  fst $ foldr (\(cle1,e1) (cle2,e2) -> if length (refus e1) < length (refus e2) && not (estPrioritaire e1) || estPrioritaire e2 then (cle1,e1) else (cle2,e2) ) (last liste) liste
        where liste = ( IntMap.toList etudiants)


    moinsInsatisfaitDansCreneau :: CleCreneau -> IntMap.IntMap Etudiant -> CleEtudiant
    moinsInsatisfaitDansCreneau cleCreneau etudiants = moinsInsatisfait $ affectesDansCreneau cleCreneau etudiants
    
    affectesDansCreneau :: CleCreneau -> IntMap.IntMap Etudiant -> IntMap.IntMap Etudiant
    affectesDansCreneau cleCreneau etudiants = IntMap.filter ( affectationCourante >>> (==) (Just cleCreneau)) etudiants

    affecterDansCreneau :: CleCreneau -> Etudiant -> Etudiant
    affecterDansCreneau cleCreneau etudiant@Etudiant{..} = etudiant{affectationCourante = Just cleCreneau, refus = List.delete cleCreneau refus}

    desaffecter etudiant@Etudiant{..} = case affectationCourante of
        Nothing -> error "double desaffectation"
        Just cleC -> if elem cleC voeux then etudiant{affectationCourante = Nothing, refus = cleC : refus} else etudiant{affectationCourante = Nothing}



    data Creneau = Creneau {capacite :: Int
                    ,etudiantsAffectes :: [CleEtudiant]
                    ,codeCreneau::CodeCreneau}             
    compareCr c1 c2 = compare (capacite c1 - (length.etudiantsAffectes) c1 )  (capacite c2 - (length.etudiantsAffectes) c2) 
    instance Show Creneau where
        show Creneau{..} = concat ["(Creneau : ", codeCreneau, " affectés :  ", show etudiantsAffectes,"\n"]
    instance Eq Creneau where
        cr1 == cr2 = codeCreneau cr1 == codeCreneau cr2
    instance Ord Creneau where 
        compare = compareCr

    nouveauCreneau :: Int -> CodeCreneau -> Creneau
    nouveauCreneau capacite codeCreneau = Creneau{..} where etudiantsAffectes = []


    --maxEnPlacesLibres :: Map.Map k Creneau -> k
    maxEnPlacesLibres creneaux = fst $ foldr (\(cle1,c1) (cle2,c2) -> if c1 > c2 then (cle1,c1) else (cle2,c2)) (head liste) liste
        where liste = ( Map.toList creneaux)
    

    estComplet Creneau{..} = capacite == length etudiantsAffectes 

        
    affecterEtudiant :: CleEtudiant -> Creneau ->  Creneau
    affecterEtudiant cle creneau@Creneau{..} =  creneau{etudiantsAffectes = cle : etudiantsAffectes}

    retirerEtudiant :: CleEtudiant -> Creneau -> Creneau
    retirerEtudiant cle creneau@Creneau{..} = creneau{etudiantsAffectes = List.delete cle etudiantsAffectes}






    data Global = Global {etudiants :: IntMap.IntMap Etudiant
                    ,creneaux :: Map CleCreneau Creneau 
                    }

    global0 :: Global 
    global0 = Global mempty mempty

    showRangDesVoeux :: Global -> String 
    showRangDesVoeux Global{..} = "Rang des voeux : " ++ show [length $ IntMap.filter (\e -> (length.refus) e == (r - 1))  etudiants| r <- [1..(length creneaux)]]
    instance Show Global where
        show global@Global{..} =  concat $ fmap show (IntMap.elems etudiants) ++ fmap show (Map.elems creneaux) ++ [showRangDesVoeux global] 


    meilleurVoeu cleEtudiant global@Global{..} = let etudiant@Etudiant{..} = etudiants IntMap.! cleEtudiant
                                                        in case (voeux List.\\ refus) of
                                                                    [] -> maxEnPlacesLibres creneaux
                                                                    (v:_) -> v

    affecter cleEtudiant global = affecterEtReorganiser cleEtudiant (meilleurVoeu cleEtudiant global) global 
    affecterEtReorganiser :: CleEtudiant -> CleCreneau -> Global -> Global 
    affecterEtReorganiser cleEtudiant cleCreneau global@Global{..} = if not $ estComplet ( creneaux Map.!  cleCreneau) 
                                                        then global1
                                                        else if (placeDisponible) 
                                                            then let cleEtudiantExclu =  moinsInsatisfaitDansCreneau cleCreneau etudiants1
                                                                     creneaux2 = Map.adjust ( retirerEtudiant cleEtudiantExclu) cleCreneau creneaux1
                                                                     etudiants2 =  IntMap.adjust desaffecter cleEtudiantExclu etudiants1
                                                                     global2 = global{creneaux = creneaux2, etudiants = etudiants2}  
                                                                     cleCreneau1 =  meilleurVoeu cleEtudiantExclu global2                                             
                                                            in affecterEtReorganiser cleEtudiantExclu cleCreneau1 global2
                                                            else error "Plus de places dans aucun creneau"
                                                            where placeDisponible = sum (fmap capacite creneaux) >= length etudiants
                                                                  creneaux1 = Map.adjust (affecterEtudiant cleEtudiant) cleCreneau creneaux 
                                                                  etudiants1 = IntMap.adjust (affecterDansCreneau cleCreneau) cleEtudiant etudiants
                                                                  global1 = global{creneaux = creneaux1,etudiants = etudiants1}

                                                                  
    ajouterEtudiant :: Etudiant -> Global -> Global                                                                
    ajouterEtudiant etudiant@Etudiant{..} global@Global{..} = 
        let nouvelleCleEtudiant = IntMap.size etudiants
            in affecter nouvelleCleEtudiant 
                                global{etudiants = IntMap.insert nouvelleCleEtudiant 
                                                            etudiant
                                                            etudiants}
    ajouterCreneau ::  CodeCreneau -> Creneau -> Global -> Global
    ajouterCreneau  cle creneau global@Global{..} = if Map.member cle creneaux 
                                                    then error $ "clé de créneau \"" ++ cle ++ "\" déjà utilisée"
                                                    else global{creneaux = Map.insert cle creneau creneaux}
    retirerCreneau :: CodeCreneau -> Global -> Global 
    retirerCreneau codeCreneau global@Global{..} = foldr affecter global1 clesEtudiantsLeses
                                where clesEtudiantsLeses = let creneau = creneaux Map.! codeCreneau in etudiantsAffectes creneau 
                                      global1 = let  creneaux1 = Map.adjust (\creneau -> creneau {capacite = 0, etudiantsAffectes = []}) codeCreneau creneaux
                                                     etudiants1 = fmap (\etudiant -> 
                                                        if affectationCourante etudiant == Just codeCreneau 
                                                        then desaffecter etudiant 
                                                        else etudiant) etudiants
                                            in global{etudiants = etudiants1, creneaux = creneaux1}

                                      
    ajouterCreneaux :: [Creneau] -> Global -> Global                                       
    ajouterCreneaux creneaux global = foldr (\cr g ->ajouterCreneau (codeCreneau cr) cr g) global creneaux

    ajouterEtudiants :: [Etudiant] -> Global -> Global
    ajouterEtudiants etudiants global =  foldr ajouterEtudiant global etudiants

    definirPrioritaires prioritaires etudiants = foldr (\cle es -> map (\etudiant -> if numero etudiant == cle 
                                                                                 then etudiant{estPrioritaire = True}
                                                                                 else etudiant)
                                                                        es)
                                                    etudiants
                                                    prioritaires

    voeuxCsv Global{..} = let ligne (_,Etudiant{..}) = concat [numero,sep,voeux !! 0, sep, voeux !! 1, sep, voeux !! 2, sep, voeux !! 3, sep, affectation,"\n"] 
                                                                where affectation = fromMaybe "" affectationCourante
                                                                      sep = "\t"
                                    in  IntMap.toList etudiants >>= ligne
    
                                                      
