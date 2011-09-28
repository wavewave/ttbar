module HEP.Physics.TTBar.Print where

import Data.List

hline :: IO ()             
hline = putStrLn "============================"    

title1 :: [String]
title1 = ["nocut", "photon-veto", "1 btag", "1 lepton", "MET > 20GeV", "hard lepton eta < 1.1, pt>20", "four tight jets", "one central b jet", "5 tight jets"]


prettyprint :: (Show a) => [String] -> [a] -> [a] -> [a] -> String
prettyprint title r0 r1 r2 = concat $ zipWith4 f title r0 r1 r2
  where f t1 r01 r11 r21 = "|" ++ t1 ++ "|" 
                           ++ (show r01) ++ "|"
                           ++ (show r11) ++ "|"
                           ++ (show r21) ++ "|" ++ "\n"
