{-# LANGUAGE NoMonomorphismRestriction, PackageImports,  
             ScopedTypeVariables #-}

module HEP.Physics.TTBar.Analysis.Combined  where

import HEP.Util.Functions

import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum,fourmomfrometaphipt,trd3)

import HEP.Physics.TTBar.Reconstruction.Combined

printMinChiSqr4AllComb :: PhyEventClassified -> IO () 
printMinChiSqr4AllComb p = 
  minChiSqr4AllComb stagedMinimizer4ChiSqrUsingSimpleAndSA p >>= print . findMinFrmChiSqrResultSetLst


printComparisonMinChiSqr4AllComb :: PhyEventClassified -> IO () 
printComparisonMinChiSqr4AllComb p = do  
  action4AllComb showEventNum (\x -> minimizerComparison x >>= showComparison) p 
  return () 


printLeptonCharge :: PhyEventClassified -> IO () 
printLeptonCharge p = do 
  showEventNum p 
  let leptoncharges = map (chargesign.charge.snd) (leptonlst p)
  if length leptoncharges == 1
    then do let [lcharge] = leptoncharges
            case lcharge of
              Plus  -> putStrLn "Plus"
              Minus -> putStrLn "Minus"
    else putStrLn "strange event"   
   


invmass_lt :: SemiLepTopBothMom -> Double
invmass_lt s = sqrt . sqr4 $ mom_lep_b s `plus` mom_lep_l s `plus` mom_lep_n s