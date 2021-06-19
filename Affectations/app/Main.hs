{-# LANGUAGE RecordWildCards  #-}
module Main where
    import Parsing
    import Lib3
    import Control.Monad.State.Lazy
    affichage (Left e) = e ++ "\n"
    affichage (Right _) = ""
    main :: IO ()
    main = do
        erreurOuEtudiants <- parserEtudiants "voeux.csv"
        erreurOuCreneaux <- parserCreneaux "capacites.csv"
        erreurOuPrioritaires <- parserPrioritaires "prioritaires.csv"
        case (erreurOuEtudiants,erreurOuCreneaux,erreurOuPrioritaires) of 
            (Right etudiants, Right creneaux, Right prioritaires) -> do
                        etatInitial <- return $ ajouterEtudiants (definirPrioritaires prioritaires etudiants) $ ajouterCreneaux creneaux global0
                        resultat <- return $  proceder etatInitial 
                        putStrLn $ showRangDesVoeux $ resultat
                        fichierVoeux "sortie.csv" resultat
            (a,b,c) -> putStr $ affichage a ++ affichage b ++ affichage c 
         
