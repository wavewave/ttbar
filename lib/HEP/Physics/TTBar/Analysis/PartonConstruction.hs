-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.TTBar.Analysis.PartonConstruction
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Decay topology parser 
-- 
-----------------------------------------------------------------------------

module HEP.Physics.TTBar.Analysis.PartonConstruction where

import           Control.Applicative
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import           Data.Maybe
import           Data.List
--
import           HEP.Physics.TTBar.Type
import           HEP.Parser.LHE.Type
import           HEP.Parser.LHE.DecayTop


getmom = pupTo4mom . pup . ptlinfo


getHadronicTop :: [DecayTop PtlIDInfo] -> (HadronicTopCollection, [DecayTop PtlIDInfo])
getHadronicTop dtops =
  let selectHadTop :: DecayTop PtlIDInfo -> Maybe (DecayTop (Int,PtlIDInfo))
      selectHadTop = matchDecayTop
                       (Decay ((1,[6]), [ Terminal (2,[5]) 
                                        , Decay ((3,[24]), [ Terminal (4,[-1,-2,-3,-4])
                                                           , Terminal (5,[1,2,3,4])    
                                                           ])
                                        ]))

      selectAntiHadTop :: DecayTop PtlIDInfo -> Maybe (DecayTop (Int,PtlIDInfo))
      selectAntiHadTop = matchDecayTop
                       (Decay ((1,[-6]), [ Terminal (2,[-5])
                                         , Decay ((3,[-24]), [ Terminal (4,[-1,-2,-3,-4])
                                                             , Terminal (5,[1,2,3,4]) 
                                                             ])
                                         ]))
      -- tlst :: [Maybe (DecayTop (Int,PtlIDInfo))], DecayTop PtlIDInfo
      -- tlst = map selectHadTop dtops
      notop' :: [(Maybe (DecayTop (Int,PtlIDInfo)), DecayTop PtlIDInfo)]
      top' :: [(Maybe (DecayTop (Int,PtlIDInfo)), DecayTop PtlIDInfo)]
      (notop',top') = (partition (isNothing . fst) . map ((,) <$> selectHadTop <*> id) ) dtops  
      tops :: [IM.IntMap PtlIDInfo]
      tops = map (IM.fromList . F.toList) . catMaybes . map fst $ top'
 
      atlst = (\x -> zip (map selectAntiHadTop x) x) (map snd notop') 
      (noatop',atop') = partition (isNothing .fst) atlst
      atops :: [IM.IntMap PtlIDInfo]
      atops = map (IM.fromList . F.toList) . catMaybes . map fst $ atop'

            
      getHadTop :: TopNumber -> IM.IntMap PtlIDInfo -> Maybe HadronicTop
      getHadTop ttyp m = do -- (Decay (pt,[Terminal pb,Decay (pW, [Terminal pq1,Terminal pq2])])) =
        pt <- getmom <$> IM.lookup 1 m
        pb <- getmom <$> IM.lookup 2 m
        pW <- getmom <$> IM.lookup 3 m
        pq1 <- getmom <$> IM.lookup 5 m
        pq2 <- getmom <$> IM.lookup 4 m
        return (HTop ttyp pt pb pW pq1 pq2)

      htop = mapMaybe (getHadTop TopParticle) tops 
      hatop = mapMaybe (getHadTop AntiTopParticle) atops      
  in  (HTColl htop hatop, map snd noatop')
 
 
getLeptonicTop :: [DecayTop PtlIDInfo] -> (LeptonicTopCollection, [DecayTop PtlIDInfo]) 
getLeptonicTop dtops = 
  let selectElecTop = matchDecayTop (Decay ((1,[6]),[Terminal (2,[5]), Decay ((3,[24]),[Terminal (4,[-11]), Terminal (5,[12])])]))
      selectMuonTop = matchDecayTop (Decay ((1,[6]),[Terminal (2,[5]), Decay ((3,[24]),[Terminal (4,[-13]), Terminal (5,[14])])]))
      selectElecAnti = matchDecayTop (Decay ((1,[-6]),[Terminal (2,[-5]), Decay ((3,[-24]),[Terminal (5,[-12]), Terminal (4,[11])])]))
      selectMuonAnti = matchDecayTop (Decay ((1,[-6]),[Terminal (2,[-5]), Decay ((3,[-24]),[Terminal (5,[-14]), Terminal (4,[13])])]))

      elst = zip (map selectElecTop dtops) dtops
      (noelec,elec) = partition (\x -> fst x == Nothing) elst
      electops = map (IM.fromList . F.toList) . catMaybes . map fst $ elec

      mlst = (\x->zip (map selectMuonTop x) x) (map snd noelec)
      (nomuon,muon) = partition (\x -> fst x == Nothing) mlst
      muontops = map (IM.fromList . F.toList) . catMaybes . map fst $  muon

      elstanti = (\x->zip (map selectElecAnti x) x) (map snd nomuon)
      (noelecanti,elecanti) = partition (\x -> fst x == Nothing) elstanti
      elecantitops = map (IM.fromList . F.toList) . catMaybes . map fst $  elecanti

      mlstanti = (\x->zip (map selectMuonAnti x) x) (map snd noelecanti)
      (nomuonanti,muonanti) = partition (\x -> fst x == Nothing) mlstanti
      muonantitops = map (IM.fromList . F.toList) . catMaybes . (map fst) $  muonanti

      
      getLepTop ttyp ltyp m = do -- (Decay (pt,[Decay (pW, [Terminal pmu, Terminal pnu]),Terminal pb])) = 
        pt  <- getmom <$> IM.lookup 1 m
        pb  <- getmom <$> IM.lookup 2 m
        pW  <- getmom <$> IM.lookup 3 m
        pl  <- getmom <$> IM.lookup 4 m
        pnu <- getmom <$> IM.lookup 5 m
        return $ LTop ttyp ltyp pt pb pW pl pnu
 
      eltop  = mapMaybe (getLepTop TopParticle ElectronType) electops  
      mltop  = mapMaybe (getLepTop TopParticle MuonType) muontops 
      elatop = mapMaybe (getLepTop AntiTopParticle ElectronType) elecantitops
      mlatop = mapMaybe (getLepTop AntiTopParticle MuonType) muonantitops

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
  
