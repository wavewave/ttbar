module Main where

import HEP.Physics.TTBar.Analysis.Polarization 

main :: IO ()
main = do 
  showLHEFileStructure "test.lhe"
