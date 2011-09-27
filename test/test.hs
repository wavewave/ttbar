{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import HEP.Physics.TTBar.Analysis.PartonConstruction
import HEP.Physics.TTBar.Analysis.Polarization 


import Text.XML.Enumerator.Parse.Util

import System.IO

import HEP.Physics.TTBar.Type

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Formatter 
import HEP.Parser.LHEParser.Parser.Enumerator

import Control.Monad.State

import HEP.Automation.MadGraph.LHESanitizer.Parse

import Numeric.LinearAlgebra

import qualified Data.Text.IO as TIO

import Data.Enumerator.Util

import Data.Enumerator hiding (map,length)
import qualified Data.Enumerator as E (map, Iteratee)
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.DecayTop

import Data.List hiding (map,length)

import Data.XML.Types

import HEP.Util.Functions

import Data.Maybe
import HROOT


printDecayTop2 :: (MonadIO m )  => E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
printDecayTop2 = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just (_,_,dtops) -> do mapM_ ( liftIO . putStrLn . show . fmap pdgid ) dtops
                               liftIO $  putStrLn "--------------"
                               printDecayTop2 

printDecayTop3 :: (MonadIO m) => TH1F -> E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
printDecayTop3 hist = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return ()
    Just maybec -> do 
      case maybec of 
        Nothing -> return () 
        Just (_,_,dtops) -> do
          let (lcoll,rem) = getLeptonicTop dtops
              (hcoll,rem') = getHadronicTop rem 
              SemiLeptonicTopPair l h = mkTopPairEvent lcoll hcoll
              topmom = l_ptop l -- getLeptonicTopMomentum tpair
              lepmom = l_plepton l 

              toplv = fourMomentumToLorentzVector topmom
              leplv = fourMomentumToLorentzVector lepmom
              bt = beta toplv
              lt = boost bt

              lepnew = lt <> leplv 
              lepnew3 =  vector3 lepnew 
          liftIO (fill1 hist (cosangle3 bt lepnew3))
          printDecayTop3 hist 
--          liftIO $ putStrLn $ show $ leplv 
          
--          liftIO $ putStrLn $ show $ lepnew 
--          liftIO $ putStrLn $ show $ lepnew3 
--           liftIO $ putStrLn $ show $ cosangle3 bt lepnew3 


--           liftIO $ putStrLn $ show $ topmom
--           liftIO $ putStrLn $ show (lt <> toplv)



toppairKind :: TopPair -> String
toppairKind (LeptonicTopPair _ _) = "LL"
toppairKind (SemiLeptonicTopPair _ _) = "LH"
toppairKind (HadronicTopPair _ _) = "HH"
toppairKind _ = "NO"

getLeptonicTopMomentum :: TopPair -> FourMomentum 
getLeptonicTopMomentum (SemiLeptonicTopPair l h) = l_ptop l 
getLeptonicTopMomentum _ = error "hey?"

testmass (Decay (pt,[Terminal pb,Decay (pW, [Terminal pmu, Terminal pnu])])) = do 
  putStrLn $ "top mass = " ++ show (sqrt (dot4 pt pt))
  putStrLn $ "b mass = " ++ show (sqrt (dot4 pb pb))
  putStrLn $ "W mass = " ++ show (sqrt (dot4 pW pW))
  putStrLn $ "mu mass = " ++ show (sqrt (dot4 pmu pmu))
  putStrLn $ "nu mass = " ++ show (sqrt (dot4 pnu pnu))

ordDecayTopEnee :: Monad m => Enumeratee (Maybe (a,b,[DecayTop PtlIDInfo])) (Maybe (a,b,[DecayTop PtlIDInfo])) m c
ordDecayTopEnee = EL.map (fmap f)
  where f (a,b,cs) = (a,b,Prelude.map mkOrdDecayTop cs)

showLHEFileStructure :: FilePath -> IO ()
showLHEFileStructure fp = do 
  putStrLn "showLHEFileStructure"

  withFile fp ReadMode $ \ih -> do 
    tcanvas <- newTCanvas "TEST" "TEST" 640 480 
    h1 <- newTH1F "test" "test" 100 (-1.2) 1.2

    let -- process :: (MonadCount m) => E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m (Int, (), ())
        process = enumZip3 countIter countMarkerIter (printDecayTop3 h1) -- printDecayTop -- printDecayTop2
    let iter = do 
          header <- textLHEHeader
          liftIO $ mapM_ (TIO.putStr) header
          parseEventIter $ decayTopEnee  =$ ordDecayTopEnee  =$  process
-- decayTopEnee =$ ordDecayTopEnee =$ process  
    r <- runStateT (parseXmlFile ih iter) (0::Int)
    putStrLn $ show r

    draw h1 "" 
    saveAs tcanvas "test.pdf" ""
 
    return ()

main :: IO ()
main = do 

  showLHEFileStructure "test.lhe"
