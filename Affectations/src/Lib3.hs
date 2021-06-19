{-# LANGUAGE RecordWildCards  #-}
module Lib3(global0,Creneau,Etudiant,Global,nouvelEtudiant,nouveauCreneau,ajouterEtudiant,ajouterCreneaux,ajouterEtudiants
    ,definirPrioritaires,showRangDesVoeux,voeuxCsv,proceder) where

    import qualified Data.IntMap.Strict as IntMap
    import qualified Data.HashMap.Strict as Map
    import qualified Data.List as List
    import Control.Category((<<<),(>>>))
    import Data.Maybe(fromMaybe,isJust,fromJust,isNothing)
    import Data.Bool(bool)
    import Data.Foldable(maximumBy,minimumBy)
    import Data.Function(on)
    import Data.STRef 
    import Control.Monad.ST


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
    aPrioriser Etudiant{..} = case voeux of
        [] -> False
        (v:_) -> estPrioritaire && affectationCourante /= Just v
    rangDeLAffectaion Etudiant{..} = case affectationCourante of
        Nothing -> length voeux + 1
        Just c -> rangDuVoeu c Etudiant{..}
    rangDuVoeu cle Etudiant{..} = fromMaybe (length voeux) (List.elemIndex cle voeux) + 1
    {-compareSatisfaction e1 e2 = case (aPrioriser e1, aPrioriser e2) of
        (True,False) -> LT 
        (False,True) -> GT
        (True,True) -> EQ -
        (False,False) -> if estPrioritaire e1 && estPrioritaire e2 
                        then EQ 
                        else case (isJust $ affectationCourante e1, isJust $ affectationCourante e2) of
                            (False,True) -> LT
                            (True,False) -> GT
                            (False,False) -> EQ 
                            (True,True) -> compare (- rangDeLAffectaion e1) (- rangDeLAffectaion e2)-}

    instance Show Etudiant where
        show Etudiant{..} = concat ["(Etudiant : ", show numero, " ", show affectationCourante, " refus : ", show refus,"\n"] 
    instance Eq Etudiant where 
        e1 == e2 = numero e1 == numero e2
    {-instance Ord Etudiant where
        compare = compareSatisfaction-}
    nouvelEtudiant :: [CodeCreneau] -> NumEtudiant-> Etudiant 
    nouvelEtudiant voeux numero = Etudiant {..} 
        where affectationCourante = Nothing
              refus = []
              estPrioritaire = False
   
    {-moinsInsatisfait :: IntMap.IntMap Etudiant -> CleEtudiant
    moinsInsatisfait etudiants =  fst $ foldr (\(cle1,e1) (cle2,e2) -> if e1 > e2 then (cle1,e1) else (cle2,e2) ) (last liste) liste
        where liste = ( IntMap.toList etudiants)


    moinsInsatisfaitDansCreneau :: CleCreneau -> IntMap.IntMap Etudiant -> CleEtudiant
    moinsInsatisfaitDansCreneau cleCreneau etudiants = moinsInsatisfait $ affectesDansCreneau cleCreneau etudiants-}
    
    affectesDansCreneau :: CleCreneau -> IntMap.IntMap Etudiant -> [Etudiant]
    affectesDansCreneau cleCreneau etudiants = filter ( affectationCourante >>> (==) (Just cleCreneau)) $ IntMap.elems etudiants

    affecterDansCreneau :: CleCreneau -> Etudiant -> Etudiant
    affecterDansCreneau cleCreneau etudiant@Etudiant{..} = etudiant{affectationCourante = Just cleCreneau, refus = List.delete cleCreneau refus}
    affecterOuRetirer maybeCle etudiant = etudiant{affectationCourante = maybeCle }

    desaffecter etudiant@Etudiant{..} = case affectationCourante of
        Nothing -> error "double desaffectation"
        Just cleC -> if elem cleC voeux then etudiant{affectationCourante = Nothing, refus = cleC : refus} else etudiant{affectationCourante = Nothing}



    data Creneau = Creneau {capacite :: Int
                    ,codeCreneau::CodeCreneau
                    ,partition :: Int
                    ,bloque :: Bool
                    ,tension :: Int}             

    instance Show Creneau where
        show Creneau{..} = concat ["(Creneau : ", codeCreneau, " affectés :  ","\n"]
    instance Eq Creneau where
        cr1 == cr2 = codeCreneau cr1 == codeCreneau cr2
    

    nouveauCreneau :: Int -> Int -> CodeCreneau -> Creneau
    nouveauCreneau partition capacite codeCreneau = Creneau{..} where bloque = False 
                                                                      tension = -capacite

    bloquerCreneau creneau = creneau{bloque = True}
    debloquerCreneau creneau = creneau{bloque = False}

    modifierTension dt creneau = creneau{tension = tension creneau - dt}


    data Global = Global {etudiants :: IntMap.IntMap Etudiant
                    ,creneaux :: Map CleCreneau Creneau 
                    }
    instance Semigroup Global where 
        _ <> g = g
                                                                  

        
    global0 :: Global 
    global0 = Global mempty mempty
    debloquerCreneaux g@Global{..} = g{creneaux = debloquerCreneau <$> creneaux}



    showRangDesVoeux :: Global -> String 
    showRangDesVoeux Global{..} = "Rang des voeux : " 
                                    ++ show [length $ IntMap.filter (rangDeLAffectaion >>> (==) r)  etudiants| r <- [1..(length creneaux)]]
                                    ++ "\n Sur le careau : " ++ show (length $ IntMap.filter (isNothing.affectationCourante)  etudiants)
    instance Show Global where
        show global@Global{..} =  concat $ fmap show (IntMap.elems etudiants) ++ fmap show (Map.elems creneaux) ++ [showRangDesVoeux global] 


    crAt cle Global{..} = creneaux Map.! cle
    etAt cle Global{..} = etudiants IntMap.! cle
    etudiantsAffectes  Global{..} cleCreneau =  IntMap.filter (affectationCourante >>> (==) (Just cleCreneau) ) etudiants
    estSature Global{..} cleCreneau  = length (etudiantsAffectes  Global{..} cleCreneau) > capacite (crAt cleCreneau Global{..})
    estComplet Global{..} cleCreneau  = length (etudiantsAffectes Global{..} cleCreneau ) >= capacite (crAt cleCreneau Global{..})

    
    data BeneficeDeplacement = BeneficeDeplacement {
             nbAffectations :: Int 
            ,nbDesaturations :: Int
            ,nbPriorisations :: Int
            ,beneficeQuantifiable :: Float 
            } deriving (Eq,Ord)

    instance Semigroup BeneficeDeplacement where
        b1 <> b2 = BeneficeDeplacement {
                   nbAffectations = nbAffectations b1 + nbAffectations b2
                   ,nbDesaturations = nbDesaturations b1 + nbDesaturations b2
                   ,nbPriorisations = nbPriorisations b1 + nbPriorisations b2
                   ,beneficeQuantifiable = beneficeQuantifiable b1 + beneficeQuantifiable b2}
    instance Monoid BeneficeDeplacement where 
        mempty = BeneficeDeplacement 0 0 0 0


    priorisations z = mempty{nbPriorisations = z}
    deplacementQuantifiable n1 n2 = mempty{beneficeQuantifiable = f n1 n2} 
        where f n1 n2 = fromIntegral n1 - fromIntegral n2 
    desaturations z = mempty{nbDesaturations = z}
    affectations z = mempty{nbAffectations = z}

    data Deplacements = Dep (Map CleEtudiant CleCreneau) 
    deplacements liste = Dep $ Map.fromList liste
    

    
    deplacer :: CleEtudiant ->  CleCreneau -> Global -> Global 
    deplacer etu arrivee  g@Global{..} = let etudiant = etAt etu g
                                             --mT = Map.adjust (modifierTension (-1) )
                                                        in g{
                                                            etudiants =   (IntMap.adjust (affecterDansCreneau arrivee )etu etudiants)
                                                            --,creneaux = foldr mT (mT arrivee creneaux) $ dropWhile((/=) arrivee) $ voeux etudiant
                                                            }
   
    beneficeMarginal :: CleEtudiant -> CleCreneau -> Global -> BeneficeDeplacement
    beneficeMarginal etu cre global = (beneficeEtudiant::BeneficeDeplacement) <> beneficeCreneau
                                            where beneficeEtudiant = (if estPrioritaire etudiant 
                                                                    then priorisations $ case (rangDeDepart,rangDArrivee) of
                                                                                            (1,1) -> 0
                                                                                            (1,_) -> -1
                                                                                            (_,1) -> 1
                                                                                            _ -> 0

                                                                    else deplacementQuantifiable rangDeDepart rangDArrivee) 
                                                                      
                                                                
                                                                      
                                                  beneficeCreneau =  case (fromMaybe False (estSature global <$> creneauDepart),estComplet global cre) of
                                                                            (False,True) -> desaturations (-1)
                                                                            (True,False) -> desaturations 1
                                                                            _ -> mempty -- bool case... mempty (isNothing creneauDepart) 
                                                  creneauDepart =  affectationCourante etudiant
                                                  etudiant = etAt etu global
                                                  rangDArrivee = rangDuVoeu cre etudiant
                                                  rangDeDepart = rangDeLAffectaion etudiant  
                                                                                         

    ajouterEtudiants :: [Etudiant] -> Global -> Global
    ajouterEtudiants etudiants global =  foldr ajouterEtudiant global etudiants

    ajouterEtudiant :: Etudiant -> Global -> Global                                                                
    ajouterEtudiant etudiant global@Global{..} = 
        let nouvelleCleEtudiant = IntMap.size etudiants
            f = if estPrioritaire etudiant then deplacer nouvelleCleEtudiant (head $ voeux etudiant) else id
            in  f
                    global{etudiants = IntMap.insert nouvelleCleEtudiant 
                                                            etudiant
                                                            etudiants
                      ,creneaux = foldr (Map.adjust (modifierTension 1)) creneaux $ voeux etudiant }
    

    definirPrioritaires prioritaires etudiants = foldr (\cle es -> map (\etudiant -> if numero etudiant == cle 
                                                                                 then etudiant{estPrioritaire = True}
                                                                                 else etudiant)
                                                                        es)
                                                    etudiants
                                                    prioritaires


    iterationC barre g@Global{..} = let
                            creneauxNonBloques = Map.filter (not.bloque) creneaux
                            nonAffectes = IntMap.filter (affectationCourante >>> isNothing) etudiants
                            
                            creneauAppelant  = let tensionCreneau Creneau{..}  =  (IntMap.filter (\e -> rangDeLAffectaion e > rangDuVoeu codeCreneau e && elem codeCreneau (voeux e)
                                                                                    )  
                                                                >>> IntMap.size) etudiants - capacite - length (etudiantsAppelables codeCreneau) 
                                                        in codeCreneau $ minimumBy (on compare tensionCreneau) $ creneauxNonBloques
                            etudiantsAppelables codeCreneau = IntMap.filter (affectationCourante >>> (/=) (Just codeCreneau)) etudiants
                            etudiantAppele = if etudiantsAppelables creneauAppelant == mempty then Nothing 
                                             else Just $ maximumBy (on compare (\e -> beneficeMarginal e creneauAppelant g)) 
                                                    $ IntMap.keys $ etudiantsAppelables creneauAppelant
                                in if (creneauxNonBloques == mempty) || isNothing etudiantAppele  
                                   then debloquerCreneaux g
                                   else if   beneficeMarginal (fromJust etudiantAppele) creneauAppelant g > barre 
                                                && not (estComplet g creneauAppelant)
                                        then iterationC barre $ deplacer (fromJust etudiantAppele) creneauAppelant 
                                                    (debloquerCreneaux g)
                                        else iterationC barre g{creneaux = Map.adjust bloquerCreneau creneauAppelant creneaux }
    iterationE g@Global{..} = let
                            
                            etudiantAppele  = let tensionCreneau cr  =  (IntMap.filter (\e -> rangDeLAffectaion e > rangDuVoeu cr e 
                                                                                    && elem cr (voeux e))  
                                                                >>> IntMap.size) etudiants - (capacite $ creneaux Map.! cr) - length (nonDans $ cr)
                                                        in fst $ maximumBy (on compare (\(_,e) -> sum $ tensionCreneau <$> voeux e)) $ IntMap.toList etudiantsAppelables
                            nonDans codeCreneau = IntMap.filter (affectationCourante >>> (/=) (Just codeCreneau)) etudiants                            
                            etudiantsAppelables = IntMap.filter (affectationCourante >>> isNothing) etudiants
                            creneauChoisi= if etudiantsAppelables == mempty then Nothing 
                                             else Just $ maximumBy (on compare (\cr -> beneficeMarginal etudiantAppele cr g)) 
                                                    $ Map.keys $ creneaux
                                in if (etudiantsAppelables== mempty) 
                                   then g
                                   else iterationE $ deplacer etudiantAppele (fromJust creneauChoisi) g
                                        
    proceder  = iterationC mempty--foldr iteration g [mempty] --,desaturations 1 <> deplacementQuantifiable 1 5
                                       







    
    


    voeuxCsv Global{..} = let ligne (_,Etudiant{..}) = concat [numero,sep,voeux !! 0, sep, voeux !! 1, sep, voeux !! 2, sep, voeux !! 3, sep, affectation,"\n"] 
                                                                where affectation = fromMaybe "" affectationCourante
                                                                      sep = "\t"
                                    in  IntMap.toList etudiants >>= ligne
    ajouterCreneaux :: [Creneau] -> Global -> Global                                       
    ajouterCreneaux creneaux global = foldr (\cr g ->ajouterCreneau (codeCreneau cr) cr g) global creneaux

    ajouterCreneau ::  CodeCreneau -> Creneau -> Global -> Global
    ajouterCreneau  cle creneau global@Global{..} = if Map.member cle creneaux 
                                                    then error $ "clé de créneau \"" ++ cle ++ "\" déjà utilisée"
                                                    else global{creneaux = Map.insert cle creneau creneaux}