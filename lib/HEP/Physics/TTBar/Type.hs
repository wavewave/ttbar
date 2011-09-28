module HEP.Physics.TTBar.Type where

import HEP.Util.Functions

data TopNumber = TopParticle | AntiTopParticle 
               deriving (Show,Eq) 

data LeptonicTopType = ElectronType | MuonType 
                     deriving (Show, Eq)

data LeptonicTop = LTop  
  { l_topnum :: TopNumber
  , l_ltoptype :: LeptonicTopType
  , l_ptop :: FourMomentum
  , l_pbot  :: FourMomentum 
  , l_pwboson :: FourMomentum 
  , l_plepton :: FourMomentum
  , l_pneutrino :: FourMomentum
  } deriving (Show,Eq)

data HadronicTop = HTop 
  { h_topnum :: TopNumber 
  , h_ptop :: FourMomentum
  , h_pbot  :: FourMomentum 
  , h_pwboson :: FourMomentum 
  , h_pjet1 :: FourMomentum
  , h_pjet2 :: FourMomentum
  } deriving (Show,Eq)

data TopPair = SemiLeptonicTopPair { sl_ltop :: LeptonicTop
                                   , sl_htop :: HadronicTop }
             | LeptonicTopPair     { l_ltop1 :: LeptonicTop 
                                   , l_ltop2 :: LeptonicTop } 
             | HadronicTopPair     { h_htop1 :: HadronicTop
                                   , h_htop2 :: HadronicTop } 

             | NotTopPair
             deriving (Show, Eq)



data LeptonicTopCollection = LTColl { elecTops :: [LeptonicTop] 
                                    , muonTops :: [LeptonicTop] 
                                    , elecAntiTops :: [LeptonicTop]
                                    , muonAntiTops :: [LeptonicTop] } 
data HadronicTopCollection = HTColl { hadTop :: [HadronicTop]
                                    , hadAntiTop :: [HadronicTop] }

