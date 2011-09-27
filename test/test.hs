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

import Numeric.LinearAlgebra hiding ((<.>))
import System.Process

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

import HEP.Storage.WebDAV

import System.FilePath

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
          case mkTopPairEvent lcoll hcoll of 
            SemiLeptonicTopPair l h -> do 
              let topmom = l_ptop l -- getLeptonicTopMomentum tpair
                  lepmom = l_plepton l 
                  toplv = fourMomentumToLorentzVector topmom
                  leplv = fourMomentumToLorentzVector lepmom
                  bt = beta toplv
                  lt = boost bt
                  lepnew = lt <> leplv 
                  lepnew3 =  vector3 lepnew 
              liftIO (fill1 hist (cosangle3 bt lepnew3))
              return ()
              -- printDecayTop3 hist 
            x -> liftIO $ putStrLn $ show x  
          printDecayTop3 hist 

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


processOneFile :: TH1F -> (E.Iteratee Event CountIO a) -> FilePath -> IO (a,Int)
processOneFile hist iter fp =  
  withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)


showLHEFileStructure :: FilePath -> [FilePath] -> IO ()
showLHEFileStructure basename fps = do 
  tcanvas <- newTCanvas "TEST" "TEST" 640 480 
  h1 <- newTH1F "test" "test" 50 (-1.2) 1.2
  let process = enumZip3 countIter countMarkerIter (printDecayTop3 h1) 
  let iter = do 
         header <- textLHEHeader
         parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process

  putStrLn "showLHEFileStructure"
  mapM_ (processOneFile h1 iter) fps   

  draw h1 "" 
  saveAs tcanvas (basename ++ "_costh" <.> "pdf") ""
  

  HROOT.delete h1 
  HROOT.delete tcanvas
  
  return ()

wdavconfig = WebDAVConfig 
           { webdav_path_wget = "/usr/local/bin/wget"
           , webdav_path_cadaver = "/Users/iankim/opt/homebrew/bin/cadaver"
           , webdav_baseurl = "http://susy.physics.lsa.umich.edu:8080/mc" 
           } 


downloadAndGunzip :: WebDAVRemoteDir -> FilePath -> IO ()
downloadAndGunzip rdir filename = do 
  fetchFile wdavconfig rdir (filename <.> "gz")
  system $ "gunzip " ++ (filename <.> "gz")
  return ()



workOneModel basename dirname = do 
  let filenames = [ basename++"_set"++show setnum++"_unweighted_events.lhe" | setnum<-[1..10] ] 
      wdavremotedir = WebDAVRemoteDir dirname

  mapM_ (downloadAndGunzip wdavremotedir) filenames
  showLHEFileStructure basename filenames 

dataset = 
  [ ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm200.0dm0.0g0.5eta1.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")  
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm400.0dm0.0g0.5eta0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm600.0dm0.0g0.5eta3.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm800.0dm0.0g0.5eta1.0_ttbarsemilep_teva_nomatch_defcut_cone0.4") 
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm1800.0qr-0.3ql0.3br1.0bl-1.0tr1.0tl-1.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2000.0qr-1.0ql1.0br5.0bl-5.0tr5.0tl-5.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2200.0qr-0.3ql0.3br1.0bl-1.0tr1.0tl-1.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2400.0qr-3.6ql3.6br3.6bl-3.6tr3.6tl-3.6_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_schmaltz_pol", "schc8vschmm420.0mp100.0g0.45np6.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_schmaltz_pol", "schc8vschmm440.0mp100.0g0.45np5.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm400.0g2.95_ttbarsemilep_teva_nomatch_defcut_cone0.4") 
  , ("ttbardecay_TEV_trip_pol", "tripm600.0g3.4_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm800.0g4.15_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm400.0gr0.75gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm800.0gr1.4gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm200.0gr0.7gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1s_pol", "c1sm200.0gr1.5gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4") ] 


dataset_nocut = 
  [ ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm200.0dm0.0g0.5eta1.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")  
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm400.0dm0.0g0.5eta0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm600.0dm0.0g0.5eta3.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm800.0dm0.0g0.5eta1.0_ttbarsemilep_teva_nomatch_nocut_cone0.4") 
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm1800.0qr-0.3ql0.3br1.0bl-1.0tr1.0tl-1.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2000.0qr-1.0ql1.0br5.0bl-5.0tr5.0tl-5.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2200.0qr-0.3ql0.3br1.0bl-1.0tr1.0tl-1.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_schanc8v_pol", "schc8vm2400.0qr-3.6ql3.6br3.6bl-3.6tr3.6tl-3.6_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_schmaltz_pol", "schc8vschmm420.0mp100.0g0.45np6.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_schmaltz_pol", "schc8vschmm440.0mp100.0g0.45np5.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm400.0g2.95_ttbarsemilep_teva_nomatch_nocut_cone0.4") 
  , ("ttbardecay_TEV_trip_pol", "tripm600.0g3.4_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm800.0g4.15_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm400.0gr0.75gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm800.0gr1.4gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm200.0gr0.7gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1s_pol", "c1sm200.0gr1.5gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4") ] 


main :: IO ()
main = do 
  let f (x,y) = workOneModel y ("paper4" </> x)
  
  mapM_ f dataset_nocut

{-
  lc <- readConfigFile "test.conf"
  lc_clientConfiguration lc 
-}


--  showLHEFileStructure ["test.lhe","test2.lhe"]
