module HEP.Physics.TTBar.Model.Mandelstam where

import Prelude hiding (subtract)
import HEP.Util.Functions hiding (beta)

data Two2TwoMomConf = TTMC { 
    pinit1 :: FourMomentum
  , pinit2 :: FourMomentum 
  , pfin1  :: FourMomentum
  , pfin2  :: FourMomentum
  }

mandelstamS :: Two2TwoMomConf -> Double 
mandelstamS (TTMC p1 p2 _ _) = sqr4 ( p1 `plus` p2 ) 

mandelstamT :: Two2TwoMomConf -> Double 
mandelstamT (TTMC p1 _ k1 _ ) = sqr4 ( p1 `subtract` k1) 

mandelstamU :: Two2TwoMomConf -> Double 
mandelstamU (TTMC p1 _ _ k2 ) = sqr4 ( p1 `subtract` k2) 
