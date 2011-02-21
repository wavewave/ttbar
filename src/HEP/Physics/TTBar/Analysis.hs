{-# LANGUAGE NoMonomorphismRestriction, PackageImports,  
             ScopedTypeVariables #-}

module HEP.Physics.TTBar.Analysis where

import Debug.Trace

import HROOT
import LHCOAnalysis hiding (FourMomentum,fourmomfrometaphipt,trd3)


printEvent :: PhyEventClassified -> IO ()
printEvent p = do putStrLn $ show p 

