module HEP.Physics.TTBar.Cuts where

import LHCOAnalysis hiding (FourMomentum,fourmomfrometaphipt,trd3)
import Data.List
import qualified Data.Iteratee as Iter
import Data.Iteratee.Util

check :: Bool -> Maybe Bool
check True  = Just True
check False = Nothing


met_event :: PhyEventClassified -> Maybe Double
met_event p = Just $ (snd.phiptmet.met) p 
 

numbjet :: PhyEventClassified -> Maybe Double 
numbjet = Just . fromIntegral . Prelude.length . bjetlst 

numjet :: PhyEventClassified -> Maybe Double 
numjet = Just . fromIntegral . Prelude.length . jetlst 


numbjet_PGS :: PhyEventClassified -> Int
numbjet_PGS = numofobj BJet  -- Prelude.length . bjetlst

numlepton_PGS :: PhyEventClassified -> Int 
numlepton_PGS p = let numm = numofobj Muon p 
                      nume = numofobj Electron p 
                  in nume + numm 


leptonlst_eta_pt p = let elst = electronlst p
                         mlst = muonlst p 
                         etapt (_,x) = (eta x,pt x)
                         
                     in  map etapt elst ++ map etapt mlst 


missingET :: PhyEventClassified -> Double
missingET = snd . phiptmet . met

cut_photon_veto p = numofobj Photon p == 0 
cut_morethanzero_bjet   p = numbjet_PGS p >= 1
cut_single_lepton p = let ne = numofobj Electron p
                          nm = numofobj Muon p
                          nt = numofobj Tau p 
                      in  ne + nm == 1 && nt == 0

cut_missing_pt ptcut p = missingET p > ptcut
cut_lepton_eta_pt etacut ptcut p =
  let (eta_, pt_) = head $ leptonlst_eta_pt p 
  in (abs eta_ < etacut) && (pt_ > ptcut) 


cut_n_jet_eta_pt n etacut ptcut p = 
  let nj  = numofobj Jet  p 
      nbj = numofobj BJet p 
  
  in if (nj + nbj) < n
     then False
     else let jlst = map snd $ jetlst p
              jlst_filtered = map pt $ filter (\x-> abs ( eta x )< etacut ) jlst 
              bjlst = map snd $ bjetlst p 
              bjlst_filtered = map pt $ filter (\x-> abs (eta x) < etacut) bjlst 
              combined = filter (>ptcut) $ sortBy (flip compare) $ jlst_filtered ++ bjlst_filtered 
          in length combined >= n 
           
           
         --  if length combined < 4 
         --    then False 
         --    else combined !! 3 > ptcut 


cut_central_bjet_eta etacut p = 
  let bjlst = map snd $ bjetlst p 
      bjlst_filtered = map pt $ filter (\x-> abs(eta x) < etacut) bjlst 
  in length bjlst_filtered >= 1 


checkall_cuts cset p = and (map (\f -> f p) cset)


comboM :: Monad m => (a-> m b) -> (a -> m c) -> a -> m (b,c)
comboM f g x = do y1 <- f x  
                  y2 <- g x 
                  return (y1,y2)


