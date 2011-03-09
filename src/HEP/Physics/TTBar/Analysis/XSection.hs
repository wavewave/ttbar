{-# LANGUAGE BangPatterns, PackageImports #-}

module HEP.Physics.TTBar.Analysis.XSection where

import Debug.Trace

import System.IO.Unsafe

import HEP.LHAPDF 
import HEP.MonteCarlo.Plain

import System.Random.Mersenne

--import Data.Array
import HEP.Physics.TTBar.Model.Exotic

import HEP.Physics.TTBar.Model.Mandelstam
import HEP.Util.Functions

data MachineType = PP | PPbar



xsecfuncTransformer :: Double       -- ^ mass 
                       -> (Two2TwoMomConf -> Double -> Double)   -- ^ sigma mc alphas   
                       -> ( (Double,Double,Double) -> Double )  -- ^ sigma (alphas,sqrts,costh)  
xsecfuncTransformer mass sigma = 
  \(alphas,s,costh) -> let mc = sqrtsToTwo2TwoMomConf mass (sqrt s) costh
                       in sigma mc alphas
                   

partonXsecCosth1DIntegration :: Double                      -- ^ mass1                                
                                -> (Two2TwoMomConf -> Double -> Double)  -- ^ cross section function (mc,alphas) 
                                -> Double                             -- ^ sqrt s 
                                -> Double                             -- ^ alphas
                                -> IO Double                             -- ^ answer
partonXsecCosth1DIntegration mass1 sigma sqrts alphas = do 
  let func x = do let costh = 2.0*x-1.0
                  return $ 2.0 * xsecfuncTransformer mass1 sigma (alphas,sqr sqrts,costh) 
  plain1DMC func 100000 


pdfXsecIntegrand :: MachineType 
                       -> (PartonType,PartonType)           -- ^ parton1, parton2
                       -> Double                            -- ^ s 
                       -> Double                            -- ^ mu
                       -> (Double -> Double -> Double )     -- ^ sigma :: (alphaS,s) -> sigma 
                       -> (Double,Double)                   -- ^ (x1,x2)
                       -> IO Double                         -- ^ result
pdfXsecIntegrand mtyp (ptyp1,ptyp2) s mu sigma (x1,x2)= do  
  let alphaS = 0.1155984 :: Double --  <- alphasPDF  mu 
--  putStrLn $ "alphaS =" ++ show alphaS
  xf1 <- xfx x1 mu ptyp1 
  xf2 <- case mtyp of 
    PP    -> ( xfx x2 mu ptyp2 )   
    PPbar -> ( xfx x2 mu (anti ptyp2) )
  let sig = sigma alphaS (x1*x2*s) 
  let r = (xf1*xf2*sig/x1/x2)
  if isNaN r 
    then putStrLn ("NaN : " ++ show (xf1,xf2,sig,r))
    else return ()
  return r

pdfXsecCosthIntegrand :: MachineType 
                         -> (PartonType,PartonType)           -- ^ parton1, parton2
                         -> Double                            -- ^ s 
                         -> Double                            -- ^ mu
                         -> ((Double,Double,Double)->Double)     -- ^ sigma :: (alphaS,s,costh) -> sigma 
                         -> (Double,Double,Double)                   -- ^ (x1,x2, (1+costh)/2)
                         -> IO Double                         -- ^ result
pdfXsecCosthIntegrand mtyp (ptyp1,ptyp2) s mu sigma (x1,x2,u)= do  
  let alphaS = 0.1155984 :: Double --  <- alphasPDF  mu 
--  putStrLn $ "alphaS =" ++ show alphaS
      costh = 2.0*u-1.0
  xf1 <- xfx x1 mu ptyp1 
  xf2 <- case mtyp of 
    PP    -> ( xfx x2 mu ptyp2 )   
    PPbar -> ( xfx x2 mu (anti ptyp2) )
  let sig = sigma (alphaS, (x1*x2*s), costh) 
  let r = (2.0*xf1*xf2*sig/x1/x2)
  if isNaN r 
    then putStrLn ("NaN : " ++ show (xf1,xf2,sig,r))
    else return ()
  return r

  
  
