module HEP.Physics.TTBar.Error where

import Data.List

import Numeric.LinearAlgebra
import HEP.Util.Functions
import HEP.Util.Combinatorics

type EtaPhiPT = (Double,Double,Double) 
type EtaPhiPTM = (Double,Double,Double,Double)

dropm :: EtaPhiPTM -> EtaPhiPT 
dropm (eta',phi',pt',_) = (eta',phi',pt')

type PXPY = (Double,Double)

data SinglePtlErrorInfo = SinglePtlErrorInfo { 
     deltaeta :: Double, 
     deltaphi :: Double, 
     deltaET  :: Double 
  } deriving (Show,Eq)


data JetError = JetError { 
    jeterr_ET_high :: (Double,Double,Double), 
    jeterr_ET_low  :: (Double,Double,Double),
    jeterr_etarange_high :: (Double,Double),
    jeterr_etarange_low  :: (Double,Double),
    jeterr_eta_high  :: Double, 
    jeterr_eta_low   :: Double, 
    jeterr_phi_high  :: Double, 
    jeterr_phi_low   :: Double
    } deriving Show

data LeptonError = LeptonError { 
    leptonerr_ET :: (Double,Double),
    leptonerr_theta  :: Double, 
    leptonerr_phi  :: Double
} 


-- Temporary
lepError :: LeptonError 
lepError = LeptonError (0.008,0.00015) 0.001 0.001  

jetError :: JetError
jetError = JetError {  
  jeterr_ET_high = (4.8, 0.89, 0.043), 
  jeterr_ET_low  = (5.6, 1.25, 0.033),  
  jeterr_etarange_high = (1.4,3.0),
  jeterr_etarange_low  = (0,1.4),
  jeterr_eta_high = 0.02,
  jeterr_eta_low  = 0.03,
  jeterr_phi_high = 0.01,
  jeterr_phi_low  = 0.02
}


mw = 80.4
mt = 174.3

deltamw :: Double 
deltamw = 2

deltamt :: Double 
deltamt = 1.5
--



pp_corr :: EtaPhiPT -> SinglePtlErrorInfo -> Matrix Double 
pp_corr ptlinfo errinfo = (4><4)  [ p0p0, p0p1, p0p2, p0p3  
                                  , p1p0, p1p1, p1p2, p1p3 
                                  , p2p0, p2p1, p2p2, p2p3
                                  , p3p0, p3p1, p3p2, p3p3 ] 
  
  where (eta,phi,eT) = ptlinfo
        dET  = deltaET  errinfo 
        deta = deltaeta errinfo
        dphi = deltaphi errinfo
        ceta = cosh eta
        seta = sinh eta
        cphi = cos  phi
        sphi = sin  phi
    
        p0p0 = dET^2 * ceta^2  + eT^2 * deta^2 *seta^2 
        p0p1 = dET^2 * ceta * cphi
        p0p2 = dET^2 * ceta * sphi
        p0p3 = (dET^2 + eT^2 * deta^2) * ceta * seta
        p1p0 = p0p1 
        p1p1 = dET^2 * cphi^2 + eT^2 * dphi^2 * sphi^2 
        p1p2 = ( dET^2 - eT^2 * dphi^2 ) * sphi * cphi
        p1p3 = dET^2 * seta * cphi 
        p2p0 = p0p2 
        p2p1 = p1p2 
        p2p2 = dET^2 * sphi^2 + eT^2*dphi^2*cphi^2 
        p2p3 = dET^2 * seta * sphi 
        p3p0 = p0p3
        p3p1 = p1p3 
        p3p2 = p2p3 
        p3p3 = dET^2 * seta^2 + eT^2 * deta^2 * ceta^2 


          
pmissp_corr :: EtaPhiPT -> SinglePtlErrorInfo -> Matrix Double 
pmissp_corr ptlinfo errinfo = (4><2)  [ p0px, p0py  
                                      , p1px, p1py
                                      , p2px, p2py          
                                      , p3px, p3py ] 
    where (eta,phi,eT) = ptlinfo
          dET  = deltaET  errinfo 
          deta = deltaeta errinfo
          dphi = deltaphi errinfo
          ceta = cosh eta
          seta = sinh eta
          cphi = cos  phi
          sphi = sin  phi
    
          p0px = -dET^2 * ceta * cphi
          p0py = -dET^2 * ceta * sphi
          p1px = -dET^2 * cphi^2 - eT^2 * dphi^2 * sphi^2 
          p1py = -( dET^2 - eT^2 * dphi^2 ) * sphi * cphi
          p2px = -( dET^2 - eT^2 * dphi^2 ) * sphi * cphi 
          p2py = -dET^2 * sphi^2 - eT^2*dphi^2*cphi^2 
          p3px = -dET^2 * seta * cphi
          p3py = -dET^2 * seta * sphi 


pmisspmiss_corr :: [(EtaPhiPT,SinglePtlErrorInfo)] -> Matrix Double
pmisspmiss_corr totalvisibleptllist = foldl' f 0 totalvisibleptllist
  where f acc (x,err) = acc + subMatrix (1,1) (2,2) (pp_corr x err)  



errorJet :: JetError -> EtaPhiPT -> SinglePtlErrorInfo
errorJet jeterror ptlinfo = SinglePtlErrorInfo deta dphi dET
  where (eta,_,et) = ptlinfo
        (ah,bh,ch)  = jeterr_ET_high jeterror
        (al,bl,cl)  = jeterr_ET_low  jeterror
        (etah_start,etah_end) = jeterr_etarange_high jeterror
        (etal_start,etal_end) = jeterr_etarange_low  jeterror
        abseta = abs eta
        (deta,dphi,dET) = 
          if abseta < etal_end && abseta > etal_start 
          then (jeterr_eta_low jeterror, jeterr_phi_low jeterror, 
                sqrt ( al^2+bl^2*et+cl^2*et^2 ) )
          else (jeterr_eta_high jeterror, jeterr_phi_high jeterror, 
                sqrt ( ah^2+bh^2*et+ch^2*et^2 ) )
               

errorLepton :: LeptonError -> EtaPhiPT -> SinglePtlErrorInfo
errorLepton leptonerror ptlinfo = SinglePtlErrorInfo deta dphi dET 
  where (eta',_,et) = ptlinfo 
        dtheta = leptonerr_theta leptonerror
        theta = acos $ etatocosth eta' 
        deta= 1.0 / (sin theta) * dtheta
        
        dphi = leptonerr_phi leptonerror
        (c,d) = leptonerr_ET leptonerror
        dET  = sqrt ( c^2*et^2+d^2*et^4 ) 

masserr :: Matrix Double 
masserr = (2><2) [ deltamw^2 , 0 
                 , 0         , deltamt^2 ] 

