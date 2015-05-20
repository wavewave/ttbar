import HEP.Physics.TTBar.Model.HeavyHiggs
import HEP.Util.Functions

getsigma x = do
  let s1 = (xsecConvGeV2Pb . fst) (sigma 400 11.82 0.1184 (x^(2::Int)))
  putStrLn (show x  ++ " " ++ show s1)
      

main = do
  mapM_ getsigma [370,375..800] 

