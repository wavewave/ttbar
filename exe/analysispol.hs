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
import Control.Applicative 

import HEP.Automation.MadGraph.LHESanitizer.Parse

import Numeric.LinearAlgebra hiding ((<.>))
import System.Process

import qualified Data.Text.IO as TIO

import Data.Enumerator.Util

import Data.Enumerator hiding (map,length,break,head)
import qualified Data.Enumerator as E (map, Iteratee)
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.DecayTop

import Data.List hiding (map,length,break,head)

import Data.XML.Types

import HEP.Util.Functions

import Data.Maybe
import HROOT

import HEP.Storage.WebDAV

import System.FilePath
import System.Directory

import Text.StringTemplate 
import Text.StringTemplate.Helpers

import Debug.Trace

import HEP.Util.Count
import Data.Enumerator.Util.Count 

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

histogramming :: (MonadIO m) => TH2D -> (TopPair -> Bool) 
              -> (TopPair -> Either String (Double,Double))
              -> E.Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
histogramming hist cutfunc valuef = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return ()
    Just maybec -> do 
      case maybec of 
        Nothing -> return () 
        Just (_,_,dtops) -> do
          let (lcoll,rem) = getLeptonicTop dtops
              (hcoll,rem') = getHadronicTop rem 
              tpair = mkTopPairEvent lcoll hcoll 
          when (cutfunc tpair) $ 
            case valuef tpair of 
              Right (vx,vy) -> liftIO (fill2 hist vx vy) >> return ()
              Left err -> (liftIO $ putStrLn $ err) >> return ()
          histogramming hist cutfunc valuef              

costhlmttAtLabFrame :: TopPair -> Either String (Double,Double) 
costhlmttAtLabFrame (SemiLeptonicTopPair l h) = 
  let topmom = l_ptop l 
      lepmom = l_plepton l 
      toplv = fourMomentumToLorentzVector topmom
      leplv = fourMomentumToLorentzVector lepmom
      bt = beta toplv
      lt = boost bt
      lepnew = lt <> leplv 
      lepnew3 =  vector3 lepnew 
      htopmom = h_ptop h
      htoplv = fourMomentumToLorentzVector htopmom 
      mttbar = invmass topmom htopmom 
  in  Right ((cosangle3 bt lepnew3), mttbar)
costhlmttAtLabFrame a = Left ("Not semilep toppair" ++ show a)

costhlmttAtCMFrame :: TopPair -> Either String (Double,Double) 
costhlmttAtCMFrame (SemiLeptonicTopPair l h) = 
  let ltopmom = l_ptop l 
      htopmom = h_ptop h 
      ttbarmom = ltopmom `plus` htopmom
      ttbarlv = fourMomentumToLorentzVector ttbarmom
      lt_ttbarlab = toRest ttbarlv 
      lepmom = l_plepton l 
      ltoplv = fourMomentumToLorentzVector ltopmom
      leplv = fourMomentumToLorentzVector lepmom
      leplv_ttbar = lt_ttbarlab <> leplv
      ltoplv_ttbar = lt_ttbarlab <> ltoplv 
      bt = beta ltoplv_ttbar 
      lt_tttbar = boost bt 
      leplv_t = lt_tttbar <> leplv_ttbar
      lep3_t = vector3 leplv_t 
      mttbar = invmass ltopmom htopmom 
  in  Right ((cosangle3 bt lep3_t), mttbar)
costhlmttAtCMFrame a = Left ("Not semilep toppair" ++ show a)

cut1 :: TopPair -> Bool 
cut1 (SemiLeptonicTopPair l h) = 
  let lep = l_plepton l 
      neu = l_pneutrino l
      lb = l_pbot l
      hb = h_pbot h
      j1 = h_pjet1 h 
      j2 = h_pjet2 h 
  in pt lep > 20 && pt lb > 20 && pt hb > 20 && pt j1 > 20 && pt j2 > 20 && pt neu > 20 
cut1 _ = False

nocut :: TopPair -> Bool 
nocut = const True

positiveLep :: TopPair -> Bool 
positiveLep (SemiLeptonicTopPair l h) = 
  case l_topnum l of 
    TopParticle -> True 
    _ -> False
positiveLep _ = False

pt (t,x,y,z) = sqrt (x^2 + y^2)

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

processOneFile :: TH2D -> (E.Iteratee Event CountIO a) -> FilePath -> IO (a,Int)
processOneFile hist iter fp = do 
  putStrLn $ "process " ++ fp   
  withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)

showLHEFileStructure :: FilePath -> [FilePath] -> IO ()
showLHEFileStructure basename fps = do 
--  tcanvas <- newTCanvas "TEST" "TEST" 640 480 
  h2 <- newTH2D "test" "test" 50 (-1.2) 1.2 10 350 1000
  let costh_bins = [ -1.0, -0.95 .. 1.0 ] 
      mtt_bins = [0,350, 400, 450, 500, 550, 600, 700, 10000] 
  setBins2 h2 (length costh_bins - 1) costh_bins (length mtt_bins -1 ) mtt_bins 
  let process = enumZip3 countIter countMarkerIter (histogramming h2 ((&&) <$> positiveLep <*> cut1)  costhlmttAtCMFrame) 
  let iter = do 
         header <- textLHEHeader
         parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process
  putStrLn "showLHEFileStructure"
  mapM_ (processOneFile h2 iter) fps   
  tfile <- newTFile (basename ++ "_binned_CM_cut1_pos" <.> "root") "NEW" "" 1
  write h2 "" 0 0 
  close tfile "" 
  HROOT.delete h2
  return ()



--  draw h2 "lego" 
{-  
  newh1 <- tH2ProjectionX (upcastTH2 h2) "newh1" 1 1 ""
--  draw newh1 "" 
  newh2 <- tH2ProjectionX (upcastTH2 h2) "newh2" 5 5 ""
--  draw newh2 "" -- "same"
  newh3 <- tH2ProjectionX (upcastTH2 h2) "newh3" 10 10 "" 
  draw newh3 "" -- "same" -}
  
--  saveAs tcanvas (basename ++ "_binned" <.> "pdf") ""
{-  HROOT.delete newh1
  HROOT.delete newh2
  HROOT.delete newh3 -}

--  HROOT.delete tcanvas


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

workOneModel basename dirname sets = do 
  let filenames = [ basename++"_set"++show setnum++"_unweighted_events.lhe" | setnum<-sets ] 
      wdavremotedir = WebDAVRemoteDir dirname

--  mapM_ (downloadAndGunzip wdavremotedir) filenames
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
  , ("ttbardecay_TEV_alvarez_pol", "schc8vm700.0qr-5.0e-2ql0.0br-5.0e-2bl0.0tr4.5tl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_alvarez_pol", "schc8vm850.0qr-8.0e-2ql0.0br-8.0e-2bl0.0tr6.0tl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm400.0g2.95_ttbarsemilep_teva_nomatch_defcut_cone0.4") 
  , ("ttbardecay_TEV_trip_pol", "tripm600.0g3.4_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm800.0g4.15_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm400.0gr0.75gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm800.0gr1.4gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm200.0gr0.7gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm400.0gr1.3gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm600.0gr1.5gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm800.0gr2.1gl0.0_ttbarsemilep_teva_nomatch_defcut_cone0.4")
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
  , ("ttbardecay_TEV_alvarez_pol", "schc8vm700.0qr-5.0e-2ql0.0br-5.0e-2bl0.0tr4.5tl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_alvarez_pol", "schc8vm850.0qr-8.0e-2ql0.0br-8.0e-2bl0.0tr6.0tl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm400.0g2.95_ttbarsemilep_teva_nomatch_nocut_cone0.4") 
  , ("ttbardecay_TEV_trip_pol", "tripm600.0g3.4_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_trip_pol", "tripm800.0g4.15_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm400.0gr0.75gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c8v_pol", "c8vm800.0gr1.4gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm200.0gr0.7gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm400.0gr1.3gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm600.0gr1.5gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1v_pol", "c1vm800.0gr2.1gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4")
  , ("ttbardecay_TEV_c1s_pol", "c1sm200.0gr1.5gl0.0_ttbarsemilep_teva_nomatch_nocut_cone0.4") ] 

dataset_test = 
  [ ("ttbardecay_TEV_FU8C1V_pol", "fu8c1vm200.0dm0.0g0.5eta1.0_ttbarsemilep_teva_nomatch_nocut_cone0.4") ]


modelnames = map (fst . break (== '_') . snd) dataset_nocut


makeLatexFigure :: STGroup String -> String -> String 
makeLatexFigure templates modelname = 
  let nocutfilename = zmangling (modelname++"_ttbarsemilep_teva_nomatch_nocut_cone0.4" ++ "_costh" ) <.> "pdf"
      defcutfilename = zmangling (modelname++"_ttbarsemilep_teva_nomatch_defcut_cone0.4" ++ "_costh" ) <.> "pdf"
--      modelname = undefined 
  in renderTemplateGroup
       templates 
       [ ("filenameNoCut",nocutfilename) 
       , ("filenameDefaultCut",defcutfilename)
       , ("modelName",modelname) ] 
       "partonlevelcompare.tex"


makeDocument :: STGroup String -> [String] -> String 
makeDocument templates modelnames = 
  let figures = map (makeLatexFigure templates) modelnames
  in renderTemplateGroup
       templates 
       [ ("contents", intercalate "\n" figures) ] 
       "document.tex"

zmangling :: String -> String
zmangling [] = [] 
zmangling (x:xs) 
  | x == '.' = "zd" ++ zmangling xs
  | x == 'z' = "zz" ++ zmangling xs   
  | otherwise = x : zmangling xs 

copyToZMangled :: FilePath -> IO ()
copyToZMangled base = do 
  putStrLn base
  copyFile ("working3" </> base ++ "_costh" <.> "pdf") ("working3" </> zmangling base ++ "_costh" <.> "pdf")



main :: IO ()
main = do 
--  putStrLn $ show modelnames 
  currdir <- getCurrentDirectory
--  templates <- directoryGroup currdir 


--  mapM_ (\x -> copyToZMangled =<< (return.snd) x  ) dataset
--  mapM_ (\x -> copyToZMangled =<< (return.snd) x  ) dataset_nocut
 
{-
  let str = makeDocument templates modelnames

  writeFile "working3/test.tex" str 
-}
  
  setCurrentDirectory "working3"

  let f (x,y) = workOneModel y ("paper4" </> x) [1..100]
  mapM_ f dataset_nocut



--  showLHEFileStructure ["test.lhe","test2.lhe"]
