{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import HEP.Physics.TTBar.Analysis.Polarization 

import Text.XML.Enumerator.Parse.Util

import System.IO

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Formatter 
import HEP.Parser.LHEParser.Parser.Enumerator

import Control.Monad.State

import HEP.Automation.MadGraph.LHESanitizer.Parse


import qualified Data.Text.IO as TIO

import Data.Enumerator.Util

import Data.Enumerator hiding (map)
import qualified Data.Enumerator as E (map, Iteratee)
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.DecayTop

import Data.List hiding (map)

import Data.XML.Types

import HEP.Util.Functions

import Data.Maybe

printDecayTop2 :: (MonadIO m )  => E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
printDecayTop2 = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just (_,_,dtops) -> liftIO $ print (Prelude.head  dtops)
--  return ()
-- do 
--          liftIO $ putStrLn $ intercalate "\n" $ Prelude.map show dtops  


printDecayTop3 :: (MonadIO m) => E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
printDecayTop3 = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return ()
    Just maybec -> do 
      case maybec of 
        Nothing -> return () 
        Just (_,_,dtops) -> do 
          liftIO (leptonicTop dtops)

{- lst = Prelude.map (matchDecayTopAndGet4Momentum (Decay (6,[Terminal 5, Decay (24,[Terminal (-13), Terminal 14])]))) 
                                dtops
              lst' = catMaybes lst 
  
          mapM_ (liftIO . testmass ) lst' -}
      printDecayTop3 

data LeptonicTopType = ElectronType | MuonType 
                     deriving (Show, Eq)


leptonicTop dtops =  do 
  let elst = Prelude.map (matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-11), Terminal 12])]))) 
                        dtops
      elst' = Prelude.map (\x->(ElectronType,x)) (catMaybes elst)
      mlst = Prelude.map (matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-13), Terminal 14])]))) 
                        dtops
      mlst' = Prelude.map (\x->(MuonType,x)) (catMaybes mlst)
  mapM_ (putStrLn . show . fst) (elst'++mlst')
--  mapM_ testmass lst'
      

getLeptonicTop :: [DecayTop PtlIDInfo] -> ([LeptonicTop], [DecayTop PtlIDInfo]) 
getLeptonicTop dtops = 
  let selectElecTop = matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-11), Terminal 12])]))
      selectMuonTop = matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-13), Terminal 14])]))
      selectElecAnti = matchDecayTopAndGet4Momentum  (Decay ((-6),[Terminal (-5), Decay ((-24),[Terminal (-12), Terminal 11])]))
      selectMuonAnti = matchDecayTopAndGet4Momentum  (Decay ((-6),[Terminal (-5), Decay ((-24),[Terminal (-14), Terminal 13])]))
      elst = zipWith (Prelude.map selectElecTop dtops) dtops
      (noelec,elec) = partition (\x -> fst x == Nothing) elst
      electops = catMaybes . (map snd) $  elec

      mlst = zipWith (Prelude.map selectMuonTop noelec) noelec
      (nomuon,muon) = partition (\x -> fst x == Nothing) mlst
      muontops = catMaybes . (map snd) $  muon

      elstanti = zipWith (Prelude.map selectElecAnti nomuon) nomuon
      (noelecanti,elecanti) = partition (\x -> fst x == Nothing) elstanti
      elecantitops = catMaybes . (map snd) $  elecanti

      mlstanti = zipWith (Prelude.map selectMuonAnti noelecanti) noelecanti
      (nomuonanti,muonanti) = partition (\x -> fst x == Nothing) mlstanti
      muonantitops = catMaybes . (map snd) $  muonanti

  in ([],[])

{-
(Decay (pt,[Terminal pb,Decay (pW, [Terminal pmu, Terminal pnu])]))


      case fst x of 


  Nothing 

Prelude.map (\x->(ElectronType,x)) (catMaybes elst)
      mlst = Prelude.map (matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-13), Terminal 14])]))) 
                        dtops
      mlst' = Prelude.map (\x->(MuonType,x)) (catMaybes mlst)
-}

getSemiLeptonicTopPair :: [DecayTop PtlIDInfo] -> Maybe SemiLeptonicTopPair
getSemiLeptonicTopPair dtops = undefined 
  


testmass (Decay (pt,[Terminal pb,Decay (pW, [Terminal pmu, Terminal pnu])])) = do 
  putStrLn $ "top mass = " ++ show (sqrt (dot4 pt pt))
  putStrLn $ "b mass = " ++ show (sqrt (dot4 pb pb))
  putStrLn $ "W mass = " ++ show (sqrt (dot4 pW pW))
  putStrLn $ "mu mass = " ++ show (sqrt (dot4 pmu pmu))
  putStrLn $ "nu mass = " ++ show (sqrt (dot4 pnu pnu))


ordDecayTopEnee :: Monad m => Enumeratee (Maybe (a,b,[DecayTop PtlIDInfo])) (Maybe (a,b,[DecayTop PtlIDInfo])) m c
ordDecayTopEnee = EL.map (fmap f)
  where f (a,b,cs) = (a,b,Prelude.map mkOrdDecayTop cs)

pupTo4mom :: (Double,Double,Double,Double,Double) -> FourMomentum 
pupTo4mom (px,py,pz,e,m) = (e,px,py,pz)

matchDecayTopAndGet4Momentum :: DecayTop PDGID -> DecayTop PtlIDInfo -> Maybe (DecayTop FourMomentum) 
matchDecayTopAndGet4Momentum (Terminal pid) (Terminal pinfo) 
  | pid == pdgid pinfo = Just . Terminal . pupTo4mom . pup . ptlinfo $ pinfo
  | otherwise = Nothing
matchDecayTopAndGet4Momentum (Decay (pid,xs)) (Decay (pinfo,ys)) 
  | pid == pdgid pinfo = do zs <- zipWithM matchDecayTopAndGet4Momentum xs ys 
                            return (Decay ((pupTo4mom . pup. ptlinfo) pinfo, zs))
matchDecayTopAndGet4Momentum _ _ = Nothing 



showLHEFileStructure :: FilePath -> IO ()
showLHEFileStructure fp = do 
  putStrLn "showLHEFileStructure"
 
  withFile fp ReadMode $ \ih -> do 
    let -- process :: (MonadCount m) => E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m (Int, (), ())
        process = enumZip3 countIter countMarkerIter printDecayTop3 -- printDecayTop -- printDecayTop2
--    let t1 :: E.Iteratee  (Maybe (c,d,[DecayTop PtlIDInfo])) m (Int, (), ()) 
--        t1 = ordDecayTopEnee =$ process
    let iter = do 
          header <- textLHEHeader
          liftIO $ mapM_ (TIO.putStr) header
        
             

          parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process

-- decayTopEnee =$ ordDecayTopEnee =$ process  

    r <- runStateT (parseXmlFile ih iter) (0::Int)
    putStrLn $ show r
    return ()

  


main :: IO ()
main = do 
  showLHEFileStructure "test.lhe"
