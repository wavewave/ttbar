module HEP.Physics.TTBar.Reconstruction.Leptonic where

import LHCOAnalysis hiding (fourmomfrometaphipt, FourMomentum)

import Numeric.LinearAlgebra 
import Numeric.GSL.Minimization 
import HEP.Util.Functions
import HEP.Physics.TTBar.Error  

-- lepton momentum : etl etal phil 
-- b momentum : etb etab phib
-- missing momentum : pxmiss pymiss

data SemiLepTopInfo = SemiLepTopInfo {
     bquark    :: EtaPhiPT, 
     lepton    :: EtaPhiPT, 
     missingPT :: PXPY, 
     otherjets :: [EtaPhiPT],
     otherleptons :: [EtaPhiPT]
  } deriving (Show,Eq)


data SemiLepExc4Mom = SemiLepExc4Mom { 
     bquark_4mom :: FourMomentum, 
     lepton_4mom :: FourMomentum, 
     neutrino_4mom :: FourMomentum
  } deriving (Show,Eq)

-- (etal,phil,ptl) 
-- (etab,phib,ptb)                    
                   
plmu :: EtaPhiPT -> FourMomentum 
plmu = fourmomfrometaphipt

pbmu :: EtaPhiPT -> FourMomentum 
pbmu = fourmomfrometaphipt 

pnumu :: PXPY -> (Double,Double) -> FourMomentum 
pnumu (pxmiss, pymiss) (pmiss0, pmiss3) = (pmiss0,pxmiss,pymiss,pmiss3) 

cnstrctSemiLepExc4Mom :: SemiLepTopInfo -> (Double,Double) 
                         -> SemiLepExc4Mom
cnstrctSemiLepExc4Mom tinfo (pmiss0,pmiss3) = SemiLepExc4Mom { 
  bquark_4mom = momb,  
  lepton_4mom = moml,  
  neutrino_4mom = momn
  }
  where moml = plmu  (lepton tinfo)
        momb = pbmu  (bquark tinfo)
        momn = pnumu (missingPT tinfo) (pmiss0,pmiss3) 


y0 :: SemiLepExc4Mom -> Double 
y0 einfo = sqr4 (neutrino_4mom einfo) 

y1 :: SemiLepExc4Mom -> Double 
y1 einfo = sqr4 (lepton_4mom einfo `plus` neutrino_4mom einfo) - mw^(2 :: Int) 
    
y2 :: SemiLepExc4Mom -> Double 
y2 einfo = sqr4 (lepton_4mom einfo `plus` neutrino_4mom einfo 
                 `plus` bquark_4mom einfo) - mt^(2 :: Int)


umatrix :: JetError -> LeptonError -> SemiLepTopInfo 
           -> Matrix Double 
umatrix jerr lerr sinfo = 
  fromBlocks [ [ pp_b       , 0          , pmissp_b   ,  0 ] 
             , [ 0          , pp_l       , pmissp_l   ,  0 ]  
             , [ t_pmissp_b , t_pmissp_l , pmisspmiss ,  0 ] 
             , [ 0          , 0          , 0          ,  masserr ] ]
  where b = bquark sinfo
        l = lepton sinfo 
        pp_b     = pp_corr     b (errorJet    jerr b)
        pp_l     = pp_corr     l (errorLepton lerr l)
        pmissp_b = pmissp_corr b (errorJet    jerr b)
        pmissp_l = pmissp_corr l (errorLepton lerr l)  
        t_pmissp_b = trans pmissp_b
        t_pmissp_l = trans pmissp_l
        
        js = bquark sinfo : otherjets sinfo 
        ls = lepton sinfo : otherleptons sinfo 
        
        jswitherr = map (\x -> (x,errorJet jerr x)) js 
        lswitherr = map (\x -> (x,errorLepton lerr x)) ls
        
        pmisspmiss =  pmisspmiss_corr (jswitherr ++ lswitherr) 


dydp :: SemiLepExc4Mom -> Matrix Double 
dydp einfo = 
  (3><12) [   0,   0,   0,   0,   0,   0,   0,   0, etx0, ety0,   0, 0 
          ,   0,   0,   0,   0, l10, l11, l12, l13, etx1, ety1, mw1, 0 
          , b20, b21, b22, b23, l20, l21, l22, l23, etx2, ety2, 0  , mt1 ] 
  where (pl0,pl1,pl2,pl3) = lepton_4mom einfo
        (pb0,pb1,pb2,pb3) = bquark_4mom einfo
        (pn0,pn1,pn2,pn3) = neutrino_4mom einfo
        -- pn = sqrt (pn1^(2 :: Int)+pn2^(2 :: Int)+pn3^(2 :: Int))
    
        l10 = 2.0 * (pl0 + pn0) 
        l11 = -2.0 * (pl1 + pn1)
        l12 = -2.0 * (pl2 + pn2) 
        l13 = -2.0 * (pl3 + pn3) 
        l20 = 2.0 * (pb0 + pl0 + pn0)
        l21 = -2.0 * (pb1 + pl1 + pn1)
        l22 = -2.0 * (pb2 + pl2 + pn2)
        l23 = -2.0 * (pb3 + pl3 + pn3) 
        b20 = 2.0 * (pb0 + pl0 + pn0)
        b21 = -2.0 * (pb1 + pl1 + pn1)
        b22 = -2.0 * (pb2 + pl2 + pn2)
        b23 = -2.0 * (pb3 + pl3 + pn3)
        etx0 = - 2.0 * pn1 
        ety0 = - 2.0 * pn2
        etx1 = - 2.0 * (pl1 + pn1)
        ety1 = - 2.0 * (pl2 + pn2)
        etx2 = - 2.0 * (pb1 + pl1 + pn1)
        ety2 = - 2.0 * (pb2 + pl2 + pn2)
        mw1  = -2.0 * mw
        mt1  = -2.0 * mt 
  
        
vmatrix :: JetError -> LeptonError -> SemiLepTopInfo 
           -> (Double,Double) 
           -> Matrix Double
vmatrix jerr lerr sinfo (pmiss0,pmiss3) = 
  let einfo = cnstrctSemiLepExc4Mom sinfo (pmiss0,pmiss3)
      dydpmat = dydp einfo
  in  dydpmat <> umatrix jerr lerr sinfo <> trans (dydpmat)

yvec :: SemiLepExc4Mom -> Vector Double
yvec einfo = 3 |> [ y0 einfo, y1 einfo, y2 einfo ]
       
        
chisqr :: JetError -> LeptonError -> SemiLepTopInfo 
          -> (Double,Double) 
          -> Double 
chisqr jerr lerr sinfo (pmiss0, pmiss3) = y <.> (vinv <> y)
  where v = vmatrix jerr lerr sinfo (pmiss0, pmiss3)  
        vinv = inv v
        einfo = cnstrctSemiLepExc4Mom sinfo (pmiss0, pmiss3)
        y = yvec einfo



------- Minimization of chisqr 
        
-- | output is ((pmiss0,pmiss3),chisqrmin) 
chisqr_min :: JetError -> LeptonError -> SemiLepTopInfo 
              -> IO ((Double,Double),Double) 
chisqr_min jerr lerr sinfo = do 
      let ([x0,x3],_) = minimize NMSimplex2 1E-3 30 [100,100] f [0,0]
          y = f [x0,x3]     
      return ((x0,x3),y)
  where f [x0,x3] = chisqr jerr lerr sinfo (x0,x3)
        f _ = error "(no two dimensional result) in chisqr_min"   


eventToInfo :: PhyEventClassified -> SemiLepTopInfo
eventToInfo p = SemiLepTopInfo { bquark = bq
                               , lepton = l
                               , missingPT = (ptmissx,ptmissy)
                               , otherjets = ojets 
                               , otherleptons = [] }
  where bq = etaphiptbjet $ snd.head $ bjetlst p 
        lfull = snd $ head $ leptonlst p 
      
        
        l = find_eta_phi_pt lfull
        (phi',pt') = phiptmet $ (met p)
        ptmissx = pt' * cos phi'
        ptmissy = pt' * sin phi' 
        
        jlst = jetOrBJetMerge (jetlst p) (tail $ bjetlst p)
        ojets = map (find_eta_phi_pt.snd) jlst

        jetOrBJetMerge :: [(Int,PhyObj Jet)] -> [(Int,PhyObj BJet)]
                          -> [(Int,JetBJetObj)]
        jetOrBJetMerge jl bl = 
          let jl' = map (\(x,y)->(x,JO_Jet y)) jl 
              bl' = map (\(x,y)->(x,JO_BJet y)) bl
          in  ptordering (jl'++bl')
                      
        find_eta_phi_pt :: (MomObj a) => a -> (Double,Double,Double)
        find_eta_phi_pt x = (eta x, phi x, pt x) 

