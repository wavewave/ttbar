{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Physics.TTBar.Tester.XSecTester where


import HEP.Physics.TTBar.Model.Exotic 

import HROOT
import HEP.Util.Functions

import HEP.Physics.TTBar.Analysis.XSection

import HEP.MonteCarlo.Vegas
import HEP.MonteCarlo.Plain
import HEP.LHAPDF

import Control.Monad 

import Data.Array

import System.Random.Mersenne

import HEP.Physics.TTBar.Tester.XSecTester.Common
import HEP.Physics.TTBar.Tester.XSecTester.Parton
import HEP.Physics.TTBar.Tester.XSecTester.Proton

defaultSampling = SP2D { xStart = 0.0, xEnd = 1.0, xNumBin = 1000, 
                         yStart = 0.0, yEnd = 1.0, yNumBin = 1000, 
                         numSamplePerBin = 100 }


testfun (x,y) = x * y 

testVegas = do 
  putStrLn "test vegas"
  alphaS <- initPDFgetAlphaS "cteq66" 200.0
  hist <- newhist "test" "test" (TH1FInfo 100 0.0 1.0)
  gen <- getStdGen
  
  let f (x,y) = do xfU  <- xfx x 200.0 U 
                   xfUb <- xfx x 200.0 Ubar
                   xfD  <- xfx x 200.0 D 
                   xfDb <- xfx x 200.0 Dbar
                   xfS  <- xfx x 200.0 S
                   xfSb <- xfx x 200.0 Sbar
                   xfC  <- xfx x 200.0 C
                   xfCb <- xfx x 200.0 Cbar
                   xfB  <- xfx x 200.0 B
                   xfBb <- xfx x 200.0 Bbar
                   xfG  <- xfx x 200.0 G
                   
                   yfU  <- xfx y 200.0 U 
                   
                   return $ xfU  
--     
--                   return (xfU + xfUb + xfD + xfDb + xfS + xfSb 
--                           + xfC + xfCb + xfB + xfBb + xfG )
      
      
      
--                   yfU  <- xfx y 200.0 U 
--                   sigma <- totalXSec_qq_SM alphaS (x*y*(1960)^2) 
--                   return $ xfU * yfU * sigma / x / y 
--                   return $ (xfU - xfUb) / x
  putStrLn "start vegas sampling"
  (pdfs,cdfs) <- vegas2DSamplingM f defaultSampling gen
  putStrLn "vegas sampling finished. Now start MC"
  r <- vegasMCIntegrationM f defaultSampling pdfs cdfs 10000 gen 
  
  putStrLn $ "result = " ++ show r
  

  mapM_ (\x -> do r <- randomGenWithCDF (fst cdfs) (0.0,1.0) gen  
                  fill hist r) $ [1..10000]
  c1 <- newTCanvas "test" "test" 640 480 
  
  draw hist "" 
  
  saveAs c1 "test.pdf" ""
  
vegasMCxsec = do 
  putStrLn "vegas cross section"
  alphaS <- initPDFgetAlphaS "cteq66" 200.0
  gen <- getStdGen 
  
  let fuubar = partonXsecIntegrand PPbar (U,Ubar) ((1960)^2) 200.0 (totalXSec_qq_SM)
      fubaru = partonXsecIntegrand PPbar (Ubar,U) ((1960)^2) 200.0 (totalXSec_qq_SM) 
      fddbar = partonXsecIntegrand PPbar (D,Dbar) ((1960)^2) 200.0 (totalXSec_qq_SM)
      fdbard = partonXsecIntegrand PPbar (Dbar,D) ((1960)^2) 200.0 (totalXSec_qq_SM)
      fgg    = partonXsecIntegrand PPbar (G,G)    ((1960)^2) 200.0 (totalXSec_gg_SM)

  let integrate num func = do 
        (pdfs,cdfs) <- vegas2DSamplingM func defaultSampling gen 
        putStrLn "sampling done"
        r <- vegasMCIntegrationM func defaultSampling pdfs cdfs num gen 
        return $ xsecConvGeV2Pb r
  
  [ruubar,rubaru,rddbar,rdbard,rgg] <- mapM (integrate 100000) 
                                       [fuubar,fubaru,fddbar,fdbard,fgg]
  
  (putStrLn . show )   [ruubar,rubaru,rddbar,rdbard,rgg]
  
  (putStrLn . show ) (ruubar+rubaru+rddbar+rdbard+rgg) 



plain3dMCxsec = do  
  putStrLn "plain MC 3D"
  let cmenergy = lhc7cm 
  
  alphaS <- initPDFgetAlphaS "cteq66" 200.0

  -- just SM cross section parton level

  putStrLn $ "SM uubar cross section with fixed s = " ++ show cmenergy ++ " GeV"
  let sigma = totalXSec_qq_SM  alphaS (cmenergy^2)
  putStrLn $ "sigmaSM = " ++ (show .xsecConvGeV2Pb) sigma 

  -- uubar cross section parton level 
  putStrLn "without pdf, uubar cross section just from formula, including all"
  showPartonOnly Triplet alphaS 600.0 1.0 All 

  
  -- no pdf integration, just costh integration with fixed s
 
  putStrLn "without pdf, integrate cos theta, including all"  
  uubar_wo_pdf <- plain1DMC (\x->return $ 2.0* totalXSec_qq_exotic testTripletArg All (alphaS,cmenergy^2,2.0*x-1.0)) 100000
  (putStrLn . show . xsecConvGeV2Pb) uubar_wo_pdf
  
  
  -- pdf integration
  
  let mc = flip plain3DMC 1000000
  ruubar <- mc (partonXsecCosthIntegrand PP (U,Ubar) (cmenergy^2) 200.0 (totalXSec_qq_exotic testTripletArg All ))  

  rubaru <- mc (partonXsecCosthIntegrand PP (Ubar,U) (cmenergy^2) 200.0 (totalXSec_qq_exotic testTripletArg All ))

  putStrLn "with pdf, uubar, ubaru and sum of uubar and ubaru"
  putStrLn $ "uubar = " ++ show (xsecConvGeV2Pb ruubar)
  putStrLn $ "ubaru = " ++ show (xsecConvGeV2Pb rubaru) 
  putStrLn $ "sum   = " ++ show (xsecConvGeV2Pb (ruubar+rubaru))


plainMCxsec = do 
  putStrLn "plain cross section"
  alphaS <- initPDFgetAlphaS "cteq66" 200.0
  let mc = flip plain2DMC 10000

  ruubar <- mc (partonXsecIntegrand PPbar (U,Ubar) ((1960)^2) 200.0 totalXSec_qq_SM) 
  rubaru <- mc (partonXsecIntegrand PPbar (Ubar,U) ((1960)^2) 200.0 totalXSec_qq_SM) 
  rddbar <- mc (partonXsecIntegrand PPbar (D,Dbar) ((1960)^2) 200.0 totalXSec_qq_SM)
  rdbard <- mc (partonXsecIntegrand PPbar (Dbar,D) ((1960)^2) 200.0 totalXSec_qq_SM)
  rgg    <- mc (partonXsecIntegrand PPbar (G,G)    ((1960)^2) 200.0 totalXSec_gg_SM)


  (putStrLn . show . xsecConvGeV2Pb) ruubar
  (putStrLn . show . xsecConvGeV2Pb) rubaru
  (putStrLn . show . xsecConvGeV2Pb) rddbar
  (putStrLn . show . xsecConvGeV2Pb) rdbard
  (putStrLn . show . xsecConvGeV2Pb) rgg
  
  (putStrLn . show . xsecConvGeV2Pb) (ruubar+rubaru+rddbar+rdbard+rgg)
  
  return ()
  
