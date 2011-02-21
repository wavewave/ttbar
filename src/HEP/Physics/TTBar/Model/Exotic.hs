 
--
-- Exotic Model Utility Function  
--  by Ian-Woo Kim (iankim@umich.edu) 
--
-- created on 2011 Jan 12 
-- 
-- based on arXiv:0911.3237

module HEP.Physics.TTBar.Model.Exotic where

import Prelude hiding (subtract)
import HEP.Util.Functions hiding (beta)
import Numeric.GSL.Integration

import Debug.Trace

data ColorExoticType = NoExotic | Singlet | Octet | Triplet | Sextet
                     deriving (Show,Eq)
      
data CoeffType = Zero | Two 

data PartCalculation = OnlySM | OnlyNP | OnlyInterfere | All

data Two2TwoMomConf = TTMC { 
    pinit1 :: FourMomentum
  , pinit2 :: FourMomentum 
  , pfin1  :: FourMomentum
  , pfin2  :: FourMomentum
  }

data ModelParameters = MP { 
    exoticType :: ColorExoticType
  , mTop    :: Double
  , mPhi    :: Double
  , gStrong :: Double 
  , yS      :: Double 
  , yP      :: Double 
  }

mandelstamS :: Two2TwoMomConf -> Double 
mandelstamS (TTMC p1 p2 _ _) = sqr4 ( p1 `plus` p2 ) 

mandelstamT :: Two2TwoMomConf -> Double 
mandelstamT (TTMC p1 _ k1 _ ) = sqr4 ( p1 `subtract` k1) 

mandelstamU :: Two2TwoMomConf -> Double 
mandelstamU (TTMC p1 _ _ k2 ) = sqr4 ( p1 `subtract` k2) 


colorFactor :: ColorExoticType -> CoeffType -> Double
colorFactor NoExotic Zero = 0.0
colorFactor Singlet Zero  = 4.0
colorFactor Octet   Zero  = -2.0/3.0
colorFactor Triplet Zero  = 1.0 
colorFactor Sextet  Zero  = 1.0 
colorFactor NoExotic Two  = 0.0
colorFactor Singlet Two   = 9.0 
colorFactor Octet   Two   = 2.0 
colorFactor Triplet Two   = 3.0/4.0 
colorFactor Sextet  Two   = 3.0/2.0 

beta mt s  = sqrt (1.0 - (4.0*mt^2 / s))

-- currently these results can be applied only to Triplet and Sextets .

sumM2 pcalc c0 c2 gs y s tt ut mt tphi uphi = 
  let onlysm    = (16.0 * gs^4 ) / (s^2) * ( ut^2 + tt^2 + 2.0*s*mt^2)  
      onlyinter = 8.0 * c0 * gs^2 * y^2 * (s * mt^2 + ut^2 ) / ( s * uphi ) 
      onlynp    = c2 * 4.0 * y^4 * ut^2 / (uphi^2 ) 
  in  case pcalc of 
    OnlySM        -> onlysm 
    OnlyNP        -> onlynp 
    OnlyInterfere -> onlyinter 
    All           -> onlysm + onlynp + onlyinter 
    

dsigma s beta sumM2 = 0.25 * (1.0/9.0)* beta / (32.0*pi*s) * sumM2 

sumM2Wcosth pcalc c0 c2 gs y s mt uphi bcosth = 
  let m = mt / sqrt s
      onlysm    = (8.0*gs^4)*(1.0 + bcosth^2 + 4*m^2) 
      onlyinter = 2.0*y^2*gs^2*c0*s*((1.0+bcosth)^2 + 4.0*m^2)/uphi
      onlynp    = y^4*c2*s^2*((1.0+bcosth)^2)/(uphi^2) 
  in  case pcalc of
    OnlySM -> onlysm 
    OnlyNP -> onlynp 
    OnlyInterfere -> onlyinter 
    All    -> onlysm + onlynp + onlyinter 
    
 

dsigma_dcosth_from_mom :: ModelParameters -> PartCalculation
                          -> Two2TwoMomConf -> Double 
dsigma_dcosth_from_mom param@(MP typ mt mphi gs ys yp) pcalc mc@(TTMC p1 p2 k1 k2) = 
  let s    = mandelstamS mc 
      t    = mandelstamT mc 
      u    = mandelstamU mc 
      tt   = t - mt^2
      tphi = t - mphi^2
      ut   = u - mt^2
      uphi = u - mphi^2 
      y    = sqrt ( ys^2 + yp^2 ) 
      bet  = beta mt s 
      c0   = colorFactor typ Zero
      c2   = colorFactor typ Two
      summ2 = sumM2 pcalc c0 c2 gs y s tt ut mt tphi uphi
  in dsigma s bet summ2
      
-- | Calculate total cross section from a given cm energy
sigma_from_initial_mom :: ModelParameters
                          -> PartCalculation
                          -> Double   -- ^ Center-of-Mass energy (not individual energy !)  
                          -> Double   -- ^ total cross section
sigma_from_initial_mom param@(MP typ mt mphi gs ys yp) pcalc cmenergy = 
  let energy = cmenergy / 2.0
      pfin = sqrt ( energy^2 - mt^2 ) 
      p1 = (energy, 0, 0, energy) 
      p2 = (energy, 0, 0, -energy)
      ds_dcth costh = let sinth = sqrt ( 1.0 - costh^2 )
                          k1 = (energy, pfin*sinth , 0, pfin*costh) 
                          k2 = (energy, -pfin*sinth, 0, -pfin*costh ) 
                      in  dsigma_dcosth_from_mom param pcalc (TTMC p1 p2 k1 k2) 
    
  in fst $ integrateQAGS 1e-6 1000 (ds_dcth) (-1.0) (1.0) 


modelParameterFrom09113237 :: ColorExoticType 
                              -> Double          -- ^ mass of phi 
                              -> ModelParameters 
modelParameterFrom09113237 typ mphi = MP { 
    exoticType = typ
  , mTop    = 174.3               
  , mPhi    = mphi 
  , gStrong = alphaStoGS 0.118 
  , yS      = yvalue Sextet mphi
  , yP      = 0.0
  } 

yvalue typ mphi | typ == Triplet = mphi / 228.57 + 1.8125
                | typ == Sextet  = mphi / 257.0  + 1.28
                | otherwise      = error "don't know what to do" 

{-
modelParameterFrom09113237 typ mphi = MP { 
    exoticType = typ
  , mTop    = 174.3               
  , mPhi    = mphi 
  , gStrong = alphaStoGS 0.118 
  , yS      = yvalue Sextet mphi
  , yP      = 0.0
  } -} 


 
