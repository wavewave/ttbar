{-# LANGUAGE ExistentialQuantification #-}

module HEP.Physics.TTBar.Reconstruction.Combined where

import HEP.Parser.LHCOAnalysis.PhysObj hiding (fourmomfrometaphipt, FourMomentum)

import Numeric.LinearAlgebra 
import Numeric.GSL.Minimization 
import HEP.Util.Functions
import HEP.Util.Combinatorics 

import Data.List
import Data.Function

import HEP.Physics.TTBar.Print 
import HEP.Physics.TTBar.Error  

import HEP.Minimizer.GSLSimulatedAnnealing


import Control.Monad

data LepTopJetInfo = LepTopJetInfo { 
  leptop_lcharge :: ChargeSign, 
  leptop_b :: EtaPhiPTM, 
  leptop_l :: EtaPhiPT, 
  leptop_misspt :: PXPY
  } deriving (Show,Eq)

data HadTopJetInfo = HadTopJetInfo  { 
  hadtop_Wj1 :: EtaPhiPTM,     
  hadtop_Wj2 :: EtaPhiPTM,
  hadtop_b  :: EtaPhiPTM
  } deriving (Show,Eq)
           
data TheOthers = TheOthers { 
  theothers_jets :: [EtaPhiPTM], 
  theothers_leptons :: [EtaPhiPT]
  } deriving (Show,Eq)

data ChargeSign = Plus | Minus
                deriving (Show,Eq)

chargesign :: forall a . (Num a, Ord a) => a -> ChargeSign
chargesign ch 
  | ch >= 0 = Plus 
  | ch < 0 = Minus                       
  | otherwise = Plus

data SemiLepTopBothInfo = SemiLepTopBothInfo { 
  info_charge_l :: ChargeSign, 
  info_lep_b :: EtaPhiPTM, 
  info_lep_l :: EtaPhiPT, 
  info_lep_missPT :: PXPY, 
  info_had_Wj1 :: EtaPhiPTM, 
  info_had_Wj2 :: EtaPhiPTM, 
  info_had_b  :: EtaPhiPTM, 
  info_otherjets :: [EtaPhiPTM] , 
  info_otherleptons :: [EtaPhiPT]
  } deriving (Show, Eq) 
             
data SemiLepTopBothMom = SemiLepTopBothMom { 
  mom_lep_b :: FourMomentum, 
  mom_lep_l :: FourMomentum, 
  mom_lep_n :: FourMomentum, 
  mom_had_Wj1 :: FourMomentum, 
  mom_had_Wj2 :: FourMomentum, 
  mom_had_b :: FourMomentum 
  } deriving (Show,Eq) 

data CorrelationMatrix = CorrMat { 
    corr_pp_b  :: Matrix Double
  , corr_pp_l  :: Matrix Double
  , corr_pp_hb :: Matrix Double 
  , corr_pp_j1 :: Matrix Double
  , corr_pp_j2 :: Matrix Double
  , corr_pm_b  :: Matrix Double 
  , corr_pm_l  :: Matrix Double
  , corr_pm_hb :: Matrix Double 
  , corr_pm_j1 :: Matrix Double 
  , corr_pm_j2 :: Matrix Double
  , corr_merr  :: Matrix Double
  , corr_mm    :: Matrix Double
  } deriving Show
             
data ChiSqrMinResultSet = ChiSqrMinResultSet { 
    csmrs_p0p3   :: (Double,Double)
  , csmrs_chisqr :: Double 
  , csmrs_info   :: SemiLepTopBothInfo
  } deriving Show
                                        

mkPnumu :: PXPY -> (Double,Double) -> FourMomentum   
mkPnumu (pxmiss, pymiss) (p0miss, p3miss) = (p0miss,pxmiss,pymiss,p3miss) 

mkSemiLepTopBothMom :: SemiLepTopBothInfo -> (Double,Double) -> SemiLepTopBothMom
mkSemiLepTopBothMom tinfo (p0miss,p3miss) = 
  SemiLepTopBothMom { 
    mom_lep_b = momb, 
    mom_lep_l = moml, 
    mom_lep_n = momn, 
    mom_had_Wj1 = momWj1, 
    mom_had_Wj2 = momWj2, 
    mom_had_b = momhadb 
    }                      
    where momb = fourmomfrometaphiptm_new $ info_lep_b tinfo
          moml = fourmomfrometaphipt $ info_lep_l tinfo
          momn = mkPnumu (info_lep_missPT tinfo) (p0miss,p3miss)  
          momWj1 = fourmomfrometaphiptm_new $ info_had_Wj1 tinfo
          momWj2 = fourmomfrometaphiptm_new $ info_had_Wj2 tinfo
          momhadb = fourmomfrometaphiptm_new $ info_had_b tinfo
        
y1 ::  SemiLepTopBothMom -> Double      
y1 einfo = sqr4 (mom_lep_n einfo) 

y2 :: SemiLepTopBothMom -> Double
y2 einfo = sqr4 (mom_lep_l einfo `plus`  mom_lep_n einfo )  - mw^(2 :: Int)

y3 :: SemiLepTopBothMom -> Double 
y3 einfo = sqr4 (mom_lep_b einfo `plus` mom_lep_l einfo `plus` mom_lep_n einfo) - mt^(2 :: Int) 

y4 :: SemiLepTopBothMom -> Double 
y4 einfo = sqr4 (mom_had_Wj1 einfo `plus` mom_had_Wj2 einfo) - mw^(2 :: Int) 

y5 :: SemiLepTopBothMom -> Double
y5 einfo = sqr4 (mom_had_b einfo `plus` mom_had_Wj1 einfo `plus` mom_had_Wj2 einfo) - mt^(2 :: Int) 

yVec :: SemiLepTopBothMom -> Vector Double
yVec einfo = 5 |> [ y1 einfo, y2 einfo, y3 einfo, y4 einfo, y5 einfo]


mkCorrMat :: JetError -> LeptonError -> SemiLepTopBothInfo 
             -> CorrelationMatrix
mkCorrMat jeterr lerr sinfo = CorrMat {  
          corr_pp_b  = pp_corr b  (errorJet jeterr b) 
        , corr_pp_l  = pp_corr l  (errorLepton lerr l) 
        , corr_pp_hb = pp_corr hb (errorJet jeterr hb) 
        , corr_pp_j1 = pp_corr j1 (errorJet jeterr j1) 
        , corr_pp_j2 = pp_corr j2 (errorJet jeterr j2) 
          
        , corr_pm_b  = pmissp_corr b  (errorJet jeterr b) 
        , corr_pm_l  = pmissp_corr l  (errorLepton lerr l) 
        , corr_pm_hb = pmissp_corr hb (errorJet jeterr hb)
        , corr_pm_j1 = pmissp_corr j1 (errorJet jeterr j1)
        , corr_pm_j2 = pmissp_corr j2 (errorJet jeterr j2)
  
        , corr_merr = masserr
        , corr_mm   = pmisspmiss_corr (jswitherr ++ lswitherr) 
                
  }
  where b  = dropm $ info_lep_b   sinfo
        l  = info_lep_l   sinfo 
        hb = dropm $ info_had_b   sinfo
        j1 = dropm $ info_had_Wj1 sinfo
        j2 = dropm $ info_had_Wj2 sinfo
        
        js = b : hb : j1 : j2 : map dropm (info_otherjets sinfo)
        ls = l : info_otherleptons sinfo
          
        jswitherr = map (\x -> (x,errorJet jeterr x)) js 
        lswitherr = map (\x -> (x,errorLepton lerr x)) ls

         

uMatrix :: CorrelationMatrix -> Matrix Double
uMatrix (CorrMat pp_b pp_l pp_hb pp_j1 pp_j2 
                 pm_b pm_l pm_hb pm_j1 pm_j2
                 merr mm )
  = fromBlocks [ [ pp_b  , 0     , 0      , 0      , 0      , pm_b , 0    ] 
               , [ 0     , pp_l  , 0      , 0      , 0      , pm_l , 0    ] 
               , [ 0     , 0     , pp_hb  , 0      , 0      , pm_hb, 0    ] 
               , [ 0     , 0     , 0      , pp_j1  , 0      , pm_j1, 0    ] 
               , [ 0     , 0     , 0      , 0      , pp_j2  , pm_j2, 0    ]  
               , [ t_pm_b, t_pm_l, t_pm_hb, t_pm_j1, t_pm_j2, mm   , 0    ] 
               , [ 0     , 0     , 0      , 0      , 0      , 0    , merr ] 
               ] 
  where t_pm_b = trans pm_b 
        t_pm_l = trans pm_l 
        t_pm_hb = trans pm_hb
        t_pm_j1 = trans pm_j1 
        t_pm_j2 = trans pm_j2
        
       
dydp :: SemiLepTopBothMom -> Matrix Double 
dydp einfo = 
  (5><24) [ 0  , 0  , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , etx1, ety1, 0  , 0
          , 0  , 0  , 0  , 0  , l20, l21, l22, l23, 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , etx2, ety2, mw1, 0
          , b30, b31, b32, b33, l30, l31, l32, l33, 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0   , etx3, ety3, 0  , mt1 
          , 0  , 0  , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0   , 0   , 0   , j140, j141, j142, j143, j240, j241, j242, j243, 0   , 0   , mw1, 0  
          , 0  , 0  , 0  , 0  , 0  , 0  , 0  , 0  , hb50, hb51, hb52, hb53, j150, j151, j152, j153, j250, j251, j252, j253, 0   , 0   , 0  , mt1  
          ] 
  where {
    (pb0,pb1,pb2,pb3) = mom_lep_b einfo ; 
    (pl0,pl1,pl2,pl3) = mom_lep_l einfo ; 
    (pn0,pn1,pn2,pn3) = mom_lep_n einfo ; 
    (phb0,phb1,phb2,phb3) = mom_had_b einfo ; 
    (pj10,pj11,pj12,pj13) = mom_had_Wj1 einfo ; 
    (pj20,pj21,pj22,pj23) = mom_had_Wj2 einfo ; 
            
    etx1 = -2.0 * pn1               ; ety1 = -2.0 * pn2               ;
    etx2 = -2.0 * (pl1 + pn1)       ; ety2 = -2.0 * (pl2 + pn2)       ;
    etx3 = -2.0 * (pb1 + pl1 + pn1) ; ety3 = -2.0 * (pb2 + pl2 + pn2) ; 
        
    l20 = 2.0 * (pl0 + pn0)  ; l21 = -2.0 * (pl1 + pn1) ; 
    l22 = -2.0 * (pl2 + pn2) ; l23 = -2.0 * (pl3 + pn3) ; 
    b30 = 2.0 * (pb0 + pl0 + pn0)  ; b31 = -2.0 * (pb1 + pl1 + pn1) ; 
    b32 = -2.0 * (pb2 + pl2 + pn2) ; b33 = -2.0 * (pb3 + pl3 + pn3) ;
    l30 = 2.0 * (pb0 + pl0 + pn0)  ; l31 = -2.0 * (pb1 + pl1 + pn1) ;
    l32 = -2.0 * (pb2 + pl2 + pn2) ; l33 = -2.0 * (pb3 + pl3 + pn3) ;
    j140 = 2.0 * (pj10 + pj20)  ; j141 = -2.0 * (pj11 + pj21) ;
    j142 = -2.0 * (pj12 + pj22) ; j143 = -2.0 * (pj13 + pj23) ;
    j240 = 2.0 * (pj10 + pj20)  ; j241 = -2.0 * (pj11 + pj21) ;
    j242 = -2.0 * (pj12 + pj22) ; j243 = -2.0 * (pj13 + pj23) ; 
    hb50 = 2.0 * (phb0 + pj10 + pj20)  ; hb51 = -2.0 * (phb1 + pj11 + pj21) ;
    hb52 = -2.0 * (phb2 + pj12 + pj22) ; hb53 = -2.0 * (phb3 + pj13 + pj23) ;
    j150 = 2.0 * (phb0 + pj10 + pj20)  ; j151 = -2.0 * (phb1 + pj11 + pj21) ;
    j152 = -2.0 * (phb2 + pj12 + pj22) ; j153 = -2.0 * (phb3 + pj13 + pj23) ;
    j250 = 2.0 * (phb0 + pj10 + pj20)  ; j251 = -2.0 * (phb1 + pj11 + pj21) ;
    j252 = -2.0 * (phb2 + pj12 + pj22) ; j253 = -2.0 * (phb3 + pj13 + pj23) ;
        
    mw1 = -2.0 * mw ; mt1 = -2.0 * mt ;
    }

vMatrix :: Matrix Double -> Matrix Double -> Matrix Double 
vMatrix u dydpmat = dydpmat <> u <> trans dydpmat

chiSqr :: Vector Double -> Matrix Double -> Double 
chiSqr y v = y <.> (vinv <> y)
  where vinv = inv v


        
chiSqrMinimizable :: SemiLepTopBothInfo -> Matrix Double -> (Double,Double) -> Double 
chiSqrMinimizable sinfo umat (p0,p3) = 
  let einfo = mkSemiLepTopBothMom sinfo (p0,p3)
      vmat = vMatrix umat (dydp einfo)
      yvec = yVec einfo
  in  chiSqr yvec vmat  


minimizeChiSqrSimple :: JetError 
                        -> LeptonError 
                        -> SemiLepTopBothInfo  
                        -> MinimizeMethod       -- ^ MinimizeMethod (defined in hmatrix ) 
                        -> (Double,Double)      -- ^ initial guess 
                        -> ChiSqrMinResultSet
minimizeChiSqrSimple jeterr lerr sinfo minmethod (ip0, ip3) =
  let umat = uMatrix . (mkCorrMat jeterr lerr) $ sinfo
      tempChiSqr :: [Double] -> Double
      tempChiSqr [p0,p3] = chiSqrMinimizable sinfo umat (p0,p3)
      tempChiSqr _ = error "no double list : tempChiSqr"
      
      ([x0,x3],_) =  minimize minmethod 1E-3 30 [100,100] tempChiSqr [ip0,ip3]
      y = tempChiSqr [x0,x3] 
  in ChiSqrMinResultSet { 
       csmrs_p0p3   = (x0,x3)
     , csmrs_chisqr = y 
     , csmrs_info   = sinfo
     }
       
minimizeChiSqrSimAnn :: JetError -> LeptonError -> SemiLepTopBothInfo 
                        -> (Double,Double)
                        -> IO ChiSqrMinResultSet
minimizeChiSqrSimAnn jeterr lerr sinfo (ip0,ip3) = 
  do let umat = uMatrix . (mkCorrMat jeterr lerr) $ sinfo
         e1 = chiSqrMinimizable sinfo umat  
         m1 (x1,y1) (x2,y2) = (x1-x2)^(2 :: Int) + (y1-y2)^(2 :: Int)
     
         param= SAParam {                    
           saparam'n_tries       = 200, 
           saparam'iters_fixed_t = 100, 
           saparam'step_size     = 300.0, 
           saparam'k             = 1.0,
           saparam't_initial     = 0.1, 
           saparam'mu_t          = 2.0, 
           saparam't_min         = 1.0e-3
         }
         func = SAFunc2D { 
           safunc2d'energy  = e1, 
           safunc2d'measure = m1
     }
     r <- simanSolve2d (ip0, ip3) func param
     return $ ChiSqrMinResultSet { 
                csmrs_p0p3   = r
              , csmrs_chisqr = e1 r 
              , csmrs_info   = sinfo
              }


stagedMinimizer4ChiSqrUsingSimpleAndSA :: SemiLepTopBothInfo -> IO ChiSqrMinResultSet
stagedMinimizer4ChiSqrUsingSimpleAndSA sinfo = do 
  let (ip0,ip3) = (0,0) -- I don't want to deal with initial guess now. I will come back later
  result1 <- minimizeChiSqrSimAnn jetError lepError sinfo (ip0,ip3)
  let newp0p3 = csmrs_p0p3 result1 
  return $ minimizeChiSqrSimple jetError lepError sinfo NMSimplex2 newp0p3 


action4AllComb :: (PhyEventClassified -> IO ())   -- ^ starter
                  -> (SemiLepTopBothInfo -> IO a) -- ^ main 
                  -> PhyEventClassified -> IO [a]
action4AllComb starter iofunc p = do 
  starter p
  case cnstrctCombinatorics p of 
    Nothing -> return []
    Just r -> do let comb = (map mkSemiLepTopBothInfo) r
                 hline
                 putStrLn $ "chisqr minimum for all comb by Simulated Annealing"
                 mapM iofunc comb   


showEventNum :: PhyEventClassified -> IO () 
showEventNum p = do 
  hline
  putStrLn $ "event  : " ++ (show . eventid $ p )
  hline 



minChiSqr4AllComb :: ( SemiLepTopBothInfo -> IO ChiSqrMinResultSet ) 
                     -> PhyEventClassified -> IO [ChiSqrMinResultSet]
minChiSqr4AllComb minimizer p = action4AllComb showEventNum minimizer p 


minimizerComparison :: SemiLepTopBothInfo -> IO (ChiSqrMinResultSet,ChiSqrMinResultSet,ChiSqrMinResultSet) 
minimizerComparison sinfo = do 
  putStrLn "Minimizer Comparison" 
  let simpleMinimizerResult = minimizeChiSqrSimple jetError lepError sinfo NMSimplex2 (0,0)
  simannMinimizerResult <- minimizeChiSqrSimAnn jetError lepError sinfo (0,0) 
  combinedMinimizerResult <- stagedMinimizer4ChiSqrUsingSimpleAndSA sinfo
  return (simpleMinimizerResult, simannMinimizerResult, combinedMinimizerResult )

showComparison :: (ChiSqrMinResultSet, ChiSqrMinResultSet, ChiSqrMinResultSet) -> IO ()
showComparison (r1,r2,r3) = do 
  putStrLn $ "simple : " ++ (show (csmrs_chisqr r1))
  putStrLn $ "simann : " ++ (show (csmrs_chisqr r2)) 
  putStrLn $ "combin : " ++ (show (csmrs_chisqr r3))
      

  
findMinFrmChiSqrResultSetLst :: [ChiSqrMinResultSet] -> ChiSqrMinResultSet
findMinFrmChiSqrResultSetLst lst  
  = head $ sortBy (compare `on` csmrs_chisqr) lst 

                




testeventToInfo :: PhyEventClassified -> SemiLepTopBothInfo
testeventToInfo p = let blst = bjetlst p 
                        leptonic_bs = filter (\x->fst x == 5) blst 
                        hadronic_bs = filter (\x->fst x == 1) blst 
                        jlst = jetlst p 
                        wjets = filter (\x->fst x == 2 || fst x == 4) jlst
                        ojets = filter (\x->fst x == 3) jlst
                        llst = muonlst p 
                        leptonic_ls = filter (\x->fst x == 6) llst
                    in SemiLepTopBothInfo {
                      info_lep_b      = etaphiptm . snd $ head leptonic_bs,
                      info_lep_l      = etaphipt  . snd $ head leptonic_ls,
                      info_charge_l   = undefined,
                      info_lep_missPT = pxpyFromPhiPT . phiptmet $ met p, 
                      info_had_Wj1    = etaphiptm . snd $ wjets !! 0, 
                      info_had_Wj2    = etaphiptm . snd $ wjets !! 1,
                      info_had_b      = etaphiptm . snd $ head hadronic_bs,
                      info_otherjets  = map (etaphiptm . snd) ojets,
                      info_otherleptons = []
                      }
  


mkSemiLepTopBothInfo :: (LepTopJetInfo, HadTopJetInfo, TheOthers) 
                        -> SemiLepTopBothInfo
mkSemiLepTopBothInfo (linfo,hinfo,oinfo) = 
  SemiLepTopBothInfo {
    info_charge_l   = leptop_lcharge linfo, 
    info_lep_b      = leptop_b linfo, 
    info_lep_l      = leptop_l linfo, 
    info_lep_missPT = leptop_misspt linfo,
    info_had_Wj1    = hadtop_Wj1 hinfo,
    info_had_Wj2    = hadtop_Wj2 hinfo, 
    info_had_b      = hadtop_b hinfo,
    info_otherjets    = theothers_jets oinfo,
    info_otherleptons = theothers_leptons oinfo
    }
  

cnstrctCombinatorics :: PhyEventClassified 
                        -> Maybe  [(LepTopJetInfo, HadTopJetInfo, TheOthers)]
cnstrctCombinatorics p = do 
  guard (nleptons == 1) 
  guard ((nbjets == 1 && njets >= 3) || (nbjets == 2 && njets >= 2) ) 
  case nbjets of 
    1 -> return singleBjetCase
    2 -> return doubleBjetCase      
    _ -> error "more than 2 jet or 0 jet? in cnstrctCombinatorics"
    
  where leptons = map (\x->((etaphipt.snd) x,(chargesign.charge.snd) x)) (leptonlst p) 
        bjets   = map (etaphiptm.snd) (bjetlst p)
        jets    = map (etaphiptm.snd) (jetlst p) 
        misspt  = (pxpyFromPhiPT . phiptmet . met) p
        
        
        nleptons = length leptons
        nbjets   = length bjets
        njets    = length jets
        -- n_bjets_or_jets = nbjets + njets
        
        mkTriple (lcharge,b,l,wj1,wj2,hb,others) = (linfo,hinfo,oinfo)
          where linfo = LepTopJetInfo { leptop_lcharge = lcharge, 
                                        leptop_b = b, leptop_l = l, leptop_misspt = misspt }            
                hinfo = HadTopJetInfo { hadtop_Wj1 = wj1, hadtop_Wj2 = wj2, hadtop_b = hb } 
                oinfo = TheOthers { theothers_jets = others, theothers_leptons = [] }

        singleBjetCase = 
          let [b] = bjets
              [l] = map fst leptons
              [lcharge] = map snd leptons
              wjets_bjet_in_tjets_and_others = 
                [(lcharge,b,l,wj1,wj2,hb,others) | 
                   (tjet,others) <- combSep 3 jets, 
                   ([wj1,wj2],[hb]) <- combSep 2 tjet ]
                

          in map mkTriple wjets_bjet_in_tjets_and_others

        doubleBjetCase = 
          let [b1,b2] = bjets
              [l] = map fst leptons
              [lcharge] = map snd leptons
              bjets_perm = [(b1,b2),(b2,b1)]
              
              wjets_in_tjets_and_others = 
                [(lcharge,b,l,wj1,wj2,hb,others) | 
                 ([wj1,wj2],others) <- combSep 2 jets, 
                 (b,hb) <- bjets_perm ] 
          in map mkTriple wjets_in_tjets_and_others 


             