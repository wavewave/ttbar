module HEP.Physics.TTBar.Reconstruction.Hadronic where

import Debug.Trace

import Data.List
import Data.Function

import Numeric.LinearAlgebra

import HEP.Util.Functions
import HEP.Util.Combinatorics

import HEP.Physics.TTBar.Error


data ThreeJetsMom = ThreeJetsMom { 
    jet1_threejet :: EtaPhiPT,     
    jet2_threejet :: EtaPhiPT, 
    jet3_threejet :: EtaPhiPT
  } deriving (Show,Eq)



yj1 :: ThreeJetsMom -> Double 
yj1 (ThreeJetsMom j1 j2 j3) = sqr4 (jmom1 `plus` jmom2 `plus` jmom3)
                              - mt^2
  where jmom1 = fourmomfrometaphipt j1 
        jmom2 = fourmomfrometaphipt j2 
        jmom3 = fourmomfrometaphipt j3


yj2 :: ThreeJetsMom -> Double 
yj2 (ThreeJetsMom j1 j2 _ ) = sqr4 (jmom1 `plus` jmom2) - mt^2
  where jmom1 = fourmomfrometaphipt j1 
        jmom2 = fourmomfrometaphipt j2 



umatrixj :: JetError -> ThreeJetsMom -> Matrix Double 
umatrixj jerr threejet@(ThreeJetsMom j1 j2 j3) = 
  fromBlocks [ [ pp j1 , 0     , 0     , 0       ] 
             , [ 0     , pp j2 , 0     , 0       ] 
             , [ 0     , 0     , pp j3 , 0       ] 
             , [ 0     , 0     , 0     , masserr ] ]  
  where pp j = pp_corr j (errorJet jerr j) 
--        deltamt' = (1><1) [deltamt]
        
dydpj :: ThreeJetsMom -> Matrix Double 
dydpj threejet@(ThreeJetsMom j1 j2 j3) =
  (2><14) [ elem10, elem11, elem12, elem13, elem20, elem21, elem22, elem23, elem30, elem31, elem32, elem33, 0, elemmt
          , elem10, elem11, elem12, elem13, elem20, elem21, elem22, elem23,      0,      0,      0,      0, elemmw, 0 ]
  
  where (pj10,pj11,pj12,pj13) = fourmomfrometaphipt j1 
        (pj20,pj21,pj22,pj23) = fourmomfrometaphipt j2 
        (pj30,pj31,pj32,pj33) = fourmomfrometaphipt j3
        
        elem10 = 2.0*(pj10+pj20+pj30)
        elem11 = -2.0*(pj11+pj21+pj31) 
        elem12 = -2.0*(pj12+pj22+pj32) 
        elem13 = -2.0*(pj13+pj23+pj33) 
        
        elem20 = 2.0*(pj10+pj20+pj30)
        elem21 = -2.0*(pj11+pj21+pj31) 
        elem22 = -2.0*(pj12+pj22+pj32) 
        elem23 = -2.0*(pj13+pj23+pj33) 
        
        elem30 = 2.0*(pj10+pj20+pj30)
        elem31 = -2.0*(pj11+pj21+pj31) 
        elem32 = -2.0*(pj12+pj22+pj32) 
        elem33 = -2.0*(pj13+pj23+pj33) 

        elemmt = -2.0 * mt 
        elemmw = -2.0 * mw 

vmatrixj :: JetError -> ThreeJetsMom -> Matrix Double  
vmatrixj jerr threejet = 
  let dydpmat = dydpj threejet
  in  dydpmat <> (umatrixj jerr threejet <> trans dydpmat )
       
                         
yjvec :: ThreeJetsMom -> Vector Double
yjvec tj = 2 |> [ yj1 tj, yj2 tj ]


chisqr3jet :: JetError -> ThreeJetsMom -> Double 
chisqr3jet jerr threejet = (yjvec threejet) <.> (inv (vmatrixj jerr threejet) <> yjvec threejet)


threejetchisqr :: JetError -> FourMomentum -> [EtaPhiPT] -> [(Double,[Double])] 
threejetchisqr jerr topmom otherjet = map f otherjets_comb 
  where chisqr3func = (chisqr3jet jerr) . lstToThreeJet . fst 
        invmasstjfunc = (invtopjet topmom) . snd
    
        f x = (chisqr3func x, invmasstjfunc x)
--        tjets = map (lstToThreeJet.fst) otherjets_comb
        otherjets_comb = combSep 3 otherjet 
        

lstToThreeJet (l1:l2:l3:ls) = ThreeJetsMom l1 l2 l3

lstToEtaPhiPT :: [Double] -> EtaPhiPT 
lstToEtaPhiPT (x1:x2:x3:xs) = (x1,x2,x3)


threejetinvmass :: [EtaPhiPT] -> Double
threejetinvmass otherjet = sqrt $ sqr4 jetmom
  where jetmom = foldl1 plus $ take 3 $ map fourmomfrometaphipt $ otherjet 


threejetinvmass_chisqr :: JetError -> Double -> [EtaPhiPT] -> [(Double,Double)]
threejetinvmass_chisqr jerr chisqrcuth otherjet = 
    if fst el < chisqrcuth then [el] else []
    where 
          el = head chisqr3_invmasses

          invmass3 (ThreeJetsMom j1 j2 j3) = 
            let jmom1 = fourmomfrometaphipt j1 
                jmom2 = fourmomfrometaphipt j2 
                jmom3 = fourmomfrometaphipt j3
            in sqrt $ sqr4 (jmom1 `plus` jmom2 `plus` jmom3 ) 
         
          f x = (chisqr3jet jerr x, invmass3 x)
          chisqr3_invmasses = sortBy (compare `on` fst) $ map f all3jet_comb
        
          otherjets_comb = combSep 3 otherjet 
          tjets = map fst otherjets_comb
          tjets_comb = map (combSep 2) tjets
          
          mk3jets ([x,y],[z]) = ThreeJetsMom x y z 
          mk3jets_for_one_comb = map mk3jets 
          mk3jets_for_all = map mk3jets_for_one_comb
          
          all3jet_comb = concat (mk3jets_for_all tjets_comb)


invtopjet :: FourMomentum -> [EtaPhiPT] -> [Double]
invtopjet topmom otherjet = map (invmass topmom) jetmom 
  where otherjet_ptsorted = sortBy (flip compare `on` trd3) otherjet 
        otherjet_ptcut = filter (\x -> trd3 x > 20.0 ) otherjet_ptsorted
        
        jetmom = trace ("number of remaining jet" ++ (show $ length otherjet)) $ map fourmomfrometaphipt otherjet_ptcut       


