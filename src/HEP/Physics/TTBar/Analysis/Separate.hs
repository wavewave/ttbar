{-# LANGUAGE NoMonomorphismRestriction, PackageImports, TypeFamilies,  
             ScopedTypeVariables #-}

module HEP.Physics.TTBar.Analysis.Separate  where

import HEP.Util.Functions

import HROOT

import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum,fourmomfrometaphipt,trd3)

import HEP.Physics.TTBar.Error

import HEP.Physics.TTBar.Reconstruction.Leptonic
import HEP.Physics.TTBar.Reconstruction.Hadronic

chisqrcut :: TH1F -> Double -> SemiLepTopInfo -> ((Double,Double),Double) -> IO ()
chisqrcut hist cutval sinfo (x,y) =  
      if y < cutval 
        then do let einfo = cnstrctSemiLepExc4Mom sinfo x 
                    mtopmass = sqrt $ sqr4 (lepton_4mom einfo 
                                         `plus` neutrino_4mom einfo 
                                         `plus` bquark_4mom einfo) 
    
                fill1 hist mtopmass 
                print mtopmass
        else return ()    
          
topjetinvmass_w_chisqrcut_print :: Double -> Double -> PhyEventClassified -> IO ()
topjetinvmass_w_chisqrcut_print chisqrcutl chisqrcuth p =  
  do let sinfo = eventToInfo p 
     (_,y) <- chisqr_min jetError lepError sinfo
     putStrLn $ "chisqrcutl = " ++ show chisqrcutl
     putStrLn $ "y = " ++ show y 
     if y < chisqrcutl
       then do let -- einfo = cnstrctSemiLepExc4Mom sinfo x 
                   -- topmom = lepton_4mom einfo 
                   --         `plus` neutrino_4mom einfo 
                   --         `plus` bquark_4mom einfo  
                            
                   otherjet' = otherjets sinfo
                          
                   -- ltopinvmass = sqrt $ sqr4 topmom
                   thjresult = threejetinvmass_chisqr jetError chisqrcuth otherjet' 
                         {- htopinvmass -}
                            
               if (not.null) thjresult
                 then do let thj' = head thjresult 
                         print (eventid p, y, fst thj')
                 else return ()
                    
       else return ()
