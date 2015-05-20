import Text.Printf

import HEP.Physics.TTBar.Model.HeavyHiggs
import HEP.Util.Functions

format = printf "%d   %.5f   %.5f" 

alphaS = 0.1055320

getsigmas ecm = do
  let sA   = (xsecConvGeV2Pb . fst) (sigmaA 400 11.82 alphaS (ecm^(2::Int)))
      sQCD = (xsecConvGeV2Pb . fst) (sigmaQCD alphaS (ecm^(2::Int)))
  putStrLn  (format (round ecm :: Int) sQCD sA)
      

main = do
  mapM_ getsigmas ([370,374..800]  :: [Double])

