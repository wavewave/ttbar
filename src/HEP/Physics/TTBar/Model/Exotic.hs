 
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

import HEP.Physics.TTBar.Model.Mandelstam



data ColorExoticArg = SA { 
  sa_c0 :: Double,
  sa_c2 :: Double, 
  sa_y  :: Double, 
  sa_mt :: Double, 
  sa_mphi :: Double
  }

{-
data SextetArg = SA { 
  sa_c0 :: Double,
  sa_c2 :: Double, 
  sa_y  :: Double, 
  sa_mt :: Double, 
  sa_mphi :: Double
  } -}



testSextetArg = SA { 
  sa_c0 = colorFactor Sextet Zero, 
  sa_c2 = colorFactor Sextet Two,
  sa_y  = 1.0, 
  sa_mt = 174.3, 
  sa_mphi = 600.0
  }

testTripletArg = SA { 
  sa_c0 = colorFactor Triplet Zero, 
  sa_c2 = colorFactor Triplet Two,
  sa_y  = 1.0, 
  sa_mt = 174.3, 
  sa_mphi = 600.0
  }



data ColorExoticType = NoExotic | Singlet | Octet | Triplet | Sextet
                     deriving (Show,Eq)
      
data CoeffType = Zero | Two 

data PartCalculation = OnlySM | OnlyNP | OnlyInterfere | All


data ModelParameters = MP { 
    exoticType :: ColorExoticType
  , mTop    :: Double
  , mPhi    :: Double
  , gStrong :: Double 
  , yS      :: Double 
  , yP      :: Double 
  }



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
  let onlysm    = (16.0 * gs^(4 :: Int) ) / (s^(2 :: Int)) * ( ut^(2 :: Int) + tt^(2 :: Int) + 2.0*s*mt^(2 :: Int))  
      onlyinter = 8.0 * c0 * gs^(2 :: Int) * y^(2 :: Int) * (s * mt^(2 :: Int) + ut^(2 :: Int) ) / ( s * uphi ) 
      onlynp    = c2 * 4.0 * y^(4 :: Int) * ut^(2 :: Int) / (uphi^(2 :: Int) ) 
  in  case pcalc of 
    OnlySM        -> onlysm 
    OnlyNP        -> onlynp 
    OnlyInterfere -> onlyinter 
    All           -> onlysm + onlynp + onlyinter 
    

dsigma s beta sumM2 = 0.25 * (1.0/9.0)* beta / (32.0*pi*s) * sumM2 

sumM2Wcosth pcalc c0 c2 gs y s mt uphi bcosth = 
  let m = mt / sqrt s
      onlysm    = (8.0*gs^(4 :: Int))*(1.0 + bcosth^(2 :: Int) + 4*m^(2 :: Int)) 
      onlyinter = 2.0*y^(2 :: Int)*gs^(2 :: Int)*c0*s*((1.0+bcosth)^(2 :: Int) + 4.0*m^(2 :: Int))/uphi
      onlynp    = y^(4 :: Int)*c2*s^(2 :: Int)*((1.0+bcosth)^(2 :: Int))/(uphi^(2 :: Int)) 
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
      tt   = t - mt^(2 :: Int)
      tphi = t - mphi^(2 :: Int)
      ut   = u - mt^(2 :: Int)
      uphi = u - mphi^(2 :: Int) 
      y    = sqrt ( ys^(2 :: Int) + yp^(2 :: Int) ) 
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
      pfin = sqrt ( energy^(2 :: Int) - mt^(2 :: Int) ) 
      p1 = (energy, 0, 0, energy) 
      p2 = (energy, 0, 0, -energy)
      ds_dcth costh = let sinth = sqrt ( 1.0 - costh^(2 :: Int) )
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


 
totalXSec_qq_exotic :: ColorExoticArg -> PartCalculation 
                       -> (Double,Double,Double) -> Double
totalXSec_qq_exotic (SA c0 c2 y mt mphi) pcalc (alphaS, s, costh) =
  if s > 4.0*mt^(2::Int) 
    then let gs = sqrt (4.0*pi*alphaS)
             bet = beta mt s  
             bcosth = bet*costh
             ut = -s*(1+bcosth)/2
             uphi = ut+mt^(2::Int)-mphi^(2::Int) 
             summ2 = sumM2Wcosth pcalc c0 c2 gs y s mt uphi bcosth   
         in  dsigma s bet summ2
    else 0.0  

