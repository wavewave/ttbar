module HEP.Physics.TTBar.Model.SM where

import HEP.Physics.TTBar.Model.Mandelstam
import HEP.Util.Functions


-- | top quark pole mass 
mt :: Double 
mt = 174.3

-- | Parton-level total cross section of SM : qqbar
--   reference : Nason, Dawson, Ellis <http://141.211.99.67:8080/pub/Study/TTbarPhysics/NasonDawsonEllis1.pdf>

totalXSec_qq_SM :: Double -> Double -> Double
totalXSec_qq_SM alphaS s =
  if s > 4.0*mt^(2::Int) 
     then let rho = 4.0 * mt^(2::Int) / s
              beta = sqrt ( 1.0 - rho ) 
              fqqbar = (pi*beta*rho) / 27.0 * (2.0+rho) 
          in  alphaS^(2::Int) / mt^(2::Int) * fqqbar
     else 0 

-- | Parton-level total cross section of SM : gluglu
--   reference : Nason, Dawson, Ellis <http://141.211.99.67:8080/pub/Study/TTbarPhysics/NasonDawsonEllis1.pdf>

totalXSec_gg_SM :: Double -> Double -> Double
totalXSec_gg_SM alphaS s =
  if s > 4.0*mt^(2::Int) 
     then let rho = 4.0 * mt^(2::Int) / s
              beta = sqrt ( 1.0 - rho ) 
              fgg = (pi*beta*rho) / 192.0 
                    * ( 1.0 / beta * (rho^(2::Int) + 16.0*rho+16.0)  
                        * log ((1.0+beta)/(1.0-beta)) 
                        - 28.0 - 31.0 * rho )  
          in  alphaS^(2::Int) / mt^(2::Int) * fgg
     else 0 

-- | Parton-level differential cross section of SM : qqbar 
--   reference : PDB chap 39 with correction of beta (need to be checked. ) 

dsigma_dOmega_qqbar2ttbar_SM :: Two2TwoMomConf -> Double -> Double   
dsigma_dOmega_qqbar2ttbar_SM mc alphas =  
  let s = mandelstamS mc
      t = mandelstamT mc
      u = mandelstamU mc
      rho = 4.0 * mt^(2 :: Int) / s
      beta = sqrt (1.0-rho)
  in  beta * sqr alphas/(9.0*s^(3::Integer))*(sqr(sqr mt-t)+sqr(sqr mt-u)+2.0*sqr mt*s)
  
-- | Parton-level differential cross section of SM : qqbar 
--   reference : PDB chap 39 with correction of beta (need to be checked. ) 

dsigma_dOmega_gg2ttbar_SM :: Two2TwoMomConf -> Double -> Double 
dsigma_dOmega_gg2ttbar_SM mc alphas = 
  let s = mandelstamS mc
      t = mandelstamT mc
      u = mandelstamU mc
      rho = 4.0 * mt^(2 :: Int) / s
      beta = sqrt (1.0-rho)
  in  beta* (sqr alphas)/(32.0*s)
      *(6.0/sqr s*(sqr mt-t)*(sqr mt-u)
        -(sqr mt)*(s-4.0*sqr mt)/(3*(sqr mt-t)*(sqr mt-u))
        +4.0/3.0*((sqr mt-t)*(sqr mt-u)-2.0*sqr mt*(sqr mt+t))/sqr (sqr mt-t)
        +4.0/3.0*((sqr mt-t)*(sqr mt-u)-2.0*sqr mt*(sqr mt+u))/sqr (sqr mt-u)
        -3.0*((sqr mt-t)*(sqr mt-u)+(sqr mt)*(u-t))/(s*(sqr mt-t))
        -3.0*((sqr mt-t)*(sqr mt-u)+(sqr mt)*(t-u))/(s*(sqr mt-u)))


dsigma_dcosth_qqbar2ttbar_SM :: Two2TwoMomConf -> Double -> Double   
dsigma_dcosth_qqbar2ttbar_SM mc alphas = 2.0*pi*dsigma_dOmega_qqbar2ttbar_SM mc alphas  

dsigma_dcosth_gg2ttbar_SM :: Two2TwoMomConf -> Double -> Double   
dsigma_dcosth_gg2ttbar_SM mc alphas = 2.0*pi*dsigma_dOmega_gg2ttbar_SM mc alphas  



