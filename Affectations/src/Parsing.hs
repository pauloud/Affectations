{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing(parserEtudiants,parserCreneaux,parserPrioritaires,fichierVoeux) where
    import qualified Data.ByteString.Lazy as BL
    import Data.Csv
    import qualified Data.Vector as V
    import Lib3

   
    optionsDecodage = defaultDecodeOptions {
      decDelimiter = 9
    }
    parserEtudiants :: FilePath -> IO (Either String [Etudiant])
    parserEtudiants chemin = let f :: (String,String,String,String,String,String,String,String,String,String,String) -> Etudiant
                                 f (_,v1,_,v2,_,v3,_,v4,_,_,idEtu) = nouvelEtudiant [v1,v2,v3,v4] idEtu
        in do
            donneesCsv <- BL.readFile chemin
            case decodeWith optionsDecodage HasHeader donneesCsv of
                Left err -> return $ Left err
                Right v -> return $ Right $ f <$> V.toList v

    parserCreneaux :: FilePath -> IO (Either String [Creneau])
    parserCreneaux chemin = let f :: (String,String,Int,Int) -> Creneau
                                f(code,_,partition,capacite) = nouveauCreneau partition capacite code 
       in do
            donneesCsv <- BL.readFile chemin
            case decodeWith optionsDecodage NoHeader donneesCsv of
                Left err -> return $ Left err
                Right v -> return $ Right $ f <$> V.toList v 

    type NumEtudiant = String
    parserPrioritaires :: FilePath -> IO(Either String [NumEtudiant])
    parserPrioritaires chemin = let f :: (String,String) -> NumEtudiant
                                    f (numEtudiant,_) = numEtudiant
       in do 
            donneesCsv <- BL.readFile chemin
            case decodeWith optionsDecodage NoHeader donneesCsv of
                Left err -> return $ Left err
                Right v -> return $ Right $ f <$> V.toList v 
    
    fichierVoeux :: FilePath -> Global -> IO()
    fichierVoeux chemin global = writeFile chemin $ (showRangDesVoeux global ++ "\n" ++ voeuxCsv global) 


            