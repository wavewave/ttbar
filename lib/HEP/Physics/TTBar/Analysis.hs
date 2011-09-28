{-# LANGUAGE NoMonomorphismRestriction, PackageImports,  
             ScopedTypeVariables #-}

module HEP.Physics.TTBar.Analysis where

import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum,fourmomfrometaphipt,trd3)


printEvent :: PhyEventClassified -> IO ()
printEvent p = do putStrLn $ show p 

