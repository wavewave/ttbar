{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Physics.TTBar.Tester.XSecTester.Parton where

import HEP.Physics.TTBar.Model.Exotic 
import HEP.Physics.TTBar.Analysis.XSection

import HEP.Util.Functions
import HEP.Physics.TTBar.Tester.XSecTester.Common

showPartonOnly model alphas mphi ys pcalc = do 
  let cmenergy = 1960.0
      param = MP { 
        exoticType = model 
      , mTop    = 174.3               
      , mPhi    = mphi 
      , gStrong = alphaStoGS alphas 
      , yS      = ys
      , yP      = 0.0
      } 
    
  putStrLn $ "showPartonOnly = " ++ (show.xsecConvGeV2Pb $ sigma_from_initial_mom param pcalc cmenergy)  


{-
showPartonLevel :: IO () 
showPartonLevel = do 
  putStrLn "test crosssection (GeV^-2)"
  putStrLn " uubar -> ttbar at 1.96TeV " 
  alphaS <- initPDFgetAlphaS "cteq66" 200.0
  --let alphaS = 0.1155984

  let sigma = totalXSec_gg_SM  alphaS ((1960.0)^2)

  putStrLn $ "sigma = " ++ (show .xsecConvGeV2Pb) sigma 

  let sigma2 = totalXSec_qq_exotic testSextetArg OnlyNP (alphaS, (1960.0)^2,0.0) 
  putStrLn $ "sigma2 = " ++ (show .xsecConvGeV2Pb) sigma2 


  let cmenergy = 1960.0
      param = modelParameterFrom09113237 Sextet 600.0
  let energy = cmenergy / 2.0
      pfin = sqrt ( energy^2 - mt^2 ) 
      p1 = (energy, 0, 0, energy) 
      p2 = (energy, 0, 0, -energy)
      ds_dcth costh = let sinth = sqrt ( 1.0 - costh^2 )
                          k1 = (energy, pfin*sinth , 0, pfin*costh) 
                          k2 = (energy, -pfin*sinth, 0, -pfin*costh ) 
                      in  dsigma_dcosth_from_mom param (TTMC p1 p2 k1 k2) 
    
  putStrLn $ "test = " ++ (show.xsecConvGeV2Pb $ ds_dcth (0.5) )
-}




