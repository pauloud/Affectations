{-# LANGUAGE RecordWildCards  #-}
module Main where
    import Parsing
    import Lib
    --etudiants = zipWith nouvelEtudiant   [["c1","c2","c3"],["c2","c3","c4"],["c1","c3","c2"]] [show i| i <- [1..4]]
    --creneaux = zipWith nouveauCreneau  [1,2,2,3] ["c" ++ show i| i <- [1..4]]
    affichage (Left e) = e ++ "\n"
    affichage (Right _) = ""
    main :: IO ()
    main = do
        erreurOuEtudiants <- parserEtudiants "voeux.csv"
        erreurOuCreneaux <- parserCreneaux "capacites.csv"
        erreurOuPrioritaires <- parserPrioritaires "prioritaires.csv"
        case (erreurOuEtudiants,erreurOuCreneaux,erreurOuPrioritaires) of 
            (Right etudiants, Right creneaux, Right prioritaires) -> do
                        resultat <- return $ ajouterEtudiants (definirPrioritaires prioritaires etudiants) $ ajouterCreneaux creneaux global0
                        putStrLn $ showRangDesVoeux resultat
                        fichierVoeux "sortie.csv" resultat
            (a,b,c) -> putStr $ affichage a ++ affichage b ++ affichage c 
         
