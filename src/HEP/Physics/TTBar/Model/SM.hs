module HEP.Physics.TTBar.Model.SM where

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
--   reference : PDB chap 39

--dsigma_dOmega :: 




