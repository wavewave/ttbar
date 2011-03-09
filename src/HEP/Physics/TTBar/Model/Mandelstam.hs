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

sqrtsToTwo2TwoMomConf :: Double    -- ^ mass   (not general yet) 
                         -> Double          -- ^ sqrt of s
                         -> Double          -- ^ cos theta
                         -> Two2TwoMomConf  -- ^ resultant 2->2 mom configuration
sqrtsToTwo2TwoMomConf mass1 sqrts costh  
  = let e = sqrts /2.0 
        p1 = sqrt (sqr e - sqr mass1) 
        sinth = sqrt (1.0-sqr costh)
        mom1 = (e,0,0,e) 
        mom2 = (e,0,0,-e) 
        mom3 = (e,p1*sinth,0,p1*costh) 
        mom4 = (e,-p1*sinth,0,-p1*costh)
    in  TTMC mom1 mom2 mom3 mom4
