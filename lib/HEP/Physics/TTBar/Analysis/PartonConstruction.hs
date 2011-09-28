module HEP.Physics.TTBar.Analysis.PartonConstruction where

import HEP.Physics.TTBar.Type
import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Data.Maybe
import Data.List

getHadronicTop :: [DecayTop PtlIDInfo] -> (HadronicTopCollection, [DecayTop PtlIDInfo])
getHadronicTop dtops =
  let selectHadTop = matchDecayTopGroupAndGet4Momentum 
                       (Decay ([6], [ Terminal [5] 
                                    , Decay ([24], [ Terminal [-1,-2,-3,-4]
                                                   , Terminal [1,2,3,4]])]))
      selectAntiHadTop = matchDecayTopGroupAndGet4Momentum
                       (Decay ([-6], [ Terminal [-5]
                                     , Decay ([-24], [ Terminal [-1,-2,-3,-4]
                                                     , Terminal [1,2,3,4]])]))
      tlst = zip (map selectHadTop dtops) dtops
      (notop',top') = partition (\x -> fst x == Nothing) tlst 
      tops = catMaybes . (map fst) $ top'
 
      atlst = (\x->zip (map selectAntiHadTop x) x) (map snd notop') 
      (noatop',atop') = partition (\x -> fst x == Nothing) atlst
      atops = catMaybes . (map fst) $ atop'

      getHadTop ttyp (Decay (pt,[Terminal pb,Decay (pW, [Terminal pq1,Terminal pq2])])) = 
        HTop ttyp pt pb pW pq1 pq2
      getHadTop _ _ = error "getHadTop error"

      htop = map (getHadTop TopParticle) tops 
      hatop = map (getHadTop AntiTopParticle) atops      
  in  (HTColl htop hatop, map snd noatop')
 

getLeptonicTop :: [DecayTop PtlIDInfo] -> (LeptonicTopCollection, [DecayTop PtlIDInfo]) 
getLeptonicTop dtops = 
  let selectElecTop = matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-11), Terminal 12])]))
      selectMuonTop = matchDecayTopAndGet4Momentum  (Decay (6,[Terminal 5, Decay (24,[Terminal (-13), Terminal 14])]))
      selectElecAnti = matchDecayTopAndGet4Momentum  (Decay ((-6),[Terminal (-5), Decay ((-24),[Terminal (-12), Terminal 11])]))
      selectMuonAnti = matchDecayTopAndGet4Momentum  (Decay ((-6),[Terminal (-5), Decay ((-24),[Terminal (-14), Terminal 13])]))

      elst = zip (map selectElecTop dtops) dtops
      (noelec,elec) = partition (\x -> fst x == Nothing) elst
      electops = catMaybes . (map fst) $  elec

      mlst = (\x->zip (map selectMuonTop x) x) (map snd noelec)
      (nomuon,muon) = partition (\x -> fst x == Nothing) mlst
      muontops = catMaybes . (map fst) $  muon

      elstanti = (\x->zip (map selectElecAnti x) x) (map snd nomuon)
      (noelecanti,elecanti) = partition (\x -> fst x == Nothing) elstanti
      elecantitops = catMaybes . (map fst) $  elecanti

      mlstanti = (\x->zip (map selectMuonAnti x) x) (map snd noelecanti)
      (nomuonanti,muonanti) = partition (\x -> fst x == Nothing) mlstanti
      muonantitops = catMaybes . (map fst) $  muonanti

      
      getLepTop typ (Decay (pt,[Terminal pb,Decay (pW, [Terminal pmu, Terminal pnu])])) = 
        LTop TopParticle typ pt pb pW pmu pnu
      getLepTop _ _ = error "not matched"
 
      getAntiLepTop typ (Decay (pt,[Terminal pb,Decay (pW, [Terminal pnu, Terminal pmu])])) = 
        LTop AntiTopParticle typ pt pb pW pmu pnu
      getAntiLepTop _ _ = error "not matched"

      eltop = map (getLepTop ElectronType) electops  
      mltop = map (getLepTop MuonType) muontops 
      elatop = map (getAntiLepTop ElectronType) elecantitops
      mlatop = map (getAntiLepTop MuonType) muonantitops

  in (LTColl eltop mltop elatop mlatop, map snd nomuonanti)

mkTopPairEvent :: LeptonicTopCollection -> HadronicTopCollection -> TopPair
mkTopPairEvent lcoll hcoll = 
  case (lcoll,hcoll) of 
    (LTColl [eltop] [] [elatop] [], HTColl [] []) -> LeptonicTopPair eltop elatop
    (LTColl [eltop] [] [] [mlatop], HTColl [] []) -> LeptonicTopPair eltop mlatop
    (LTColl [] [mltop] [elatop] [], HTColl [] []) -> LeptonicTopPair mltop elatop
    (LTColl [] [mltop] [] [mlatop], HTColl [] []) -> LeptonicTopPair mltop mlatop 

    (LTColl [eltop] [] [] [], HTColl [] [hatop]) -> SemiLeptonicTopPair eltop hatop
    (LTColl [] [mltop] [] [], HTColl [] [hatop]) -> SemiLeptonicTopPair mltop hatop
    (LTColl [] [] [elatop] [], HTColl [htop] []) -> SemiLeptonicTopPair elatop htop
    (LTColl [] [] [] [mlatop], HTColl [htop] []) -> SemiLeptonicTopPair mlatop htop
    (LTColl [] [] [] [], HTColl [htop] [hatop])  -> HadronicTopPair htop hatop
    _ -> NotTopPair 
  
