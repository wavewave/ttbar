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
  } 

data HadronicTop = HTop 
  { h_topnum :: TopNumber 
  , h_ptop :: FourMomentum
  , h_pbot  :: FourMomentum 
  , h_pwboson :: FourMomentum 
  , h_plepton :: FourMomentum
  , h_pneutrino :: FourMomentum
  } 

data SemiLeptonicTopPair = SLTopPair  
   { sl_ltop :: LeptonicTop
   , sl_htop :: HadronicTop }

