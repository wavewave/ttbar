{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

module Test where 

import System.IO
import System.Posix.Files 

import Data.Maybe
import Data.Iteratee as Iter
import Data.Iteratee.Util
import Data.Iteratee.ListLike as LL 

import "mtl" Control.Monad.State

import Control.Monad.IO.Class

import LHCOAnalysis 

import Error
import Reconstruction.Combined

import FileWork

import Print 

tester :: PhyEventClassified -> IO ()
tester p = do 
  hline
  putStrLn $ "event : " ++ (show . eventid $ p ) 
  hline


{-
mytest :: PhyEventClassified -> IO ()
mytest = action (99.1661858,-63.6230) 
  where action (p0,p3) p = do 
          putStrLn "event start"
          let f_chisqr sinfo = chisqr jerr lerr sinfo (p0,p3)
              f_chisqr_min sinfo = chisqr_min jerr lerr sinfo
              f_chisqr_min_sa sinfo = chisqr_min_sa jerr lerr sinfo
          
          hline
          putStrLn $ show . eventid $ p  
          hline
             
          
          hline 
  
          let comb= (map mkSemiLepTopBothInfo) . fromJust . cnstrctCombinatorics $ p

          let comb' = cnstrctCombinatorics p 
          case comb' of 
            Nothing -> return () 
            Just r -> do let comb = (map mkSemiLepTopBothInfo) r
                         hline
                         putStrLn $ "chisqr minimum for all comb by Simulated Annealing"
                         lst_sa <- mapM f_chisqr_min_sa comb   
                         print $ lst_sa

          {-          lst <- mapM f_chisqr_min comb   
          putStrLn $ "chisqr minimum for all comb by NMSimplex"          
          print $ lst  -}
           
-}         
          
          

