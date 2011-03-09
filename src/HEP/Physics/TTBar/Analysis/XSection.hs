{-# LANGUAGE BangPatterns, PackageImports #-}

module HEP.Physics.TTBar.Analysis.XSection where

import Debug.Trace

import System.IO.Unsafe

import HEP.LHAPDF 
-- import Numeric.GSL.Integration
--import Bindings.Gsl.MonteCarloIntegration

import System.Random.Mersenne

--import Data.Array
import HEP.Physics.TTBar.Model.Exotic

data MachineType = PP | PPbar


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





partonXsecIntegrand :: MachineType 
                       -> (PartonType,PartonType)           -- ^ parton1, parton2
                       -> Double                            -- ^ s 
                       -> Double                            -- ^ mu
                       -> (Double -> Double -> Double )     -- ^ sigma :: (alphaS,s) -> sigma 
                       -> (Double,Double)                   -- ^ (x1,x2)
                       -> IO Double                         -- ^ result
partonXsecIntegrand mtyp (ptyp1,ptyp2) s mu sigma (x1,x2)= do  
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

partonXsecCosthIntegrand :: MachineType 
                            -> (PartonType,PartonType)           -- ^ parton1, parton2
                            -> Double                            -- ^ s 
                            -> Double                            -- ^ mu
                            -> ((Double,Double,Double)->Double)     -- ^ sigma :: (alphaS,s,costh) -> sigma 
                            -> (Double,Double,Double)                   -- ^ (x1,x2, (1+costh)/2)
                            -> IO Double                         -- ^ result
partonXsecCosthIntegrand mtyp (ptyp1,ptyp2) s mu sigma (x1,x2,u)= do  
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

  
  
