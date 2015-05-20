module HEP.Physics.TTBar.Model.HeavyHiggs where


import Data.Complex
-- import HEP.Physics.TTBar.Model.Mandelstam
-- import HEP.Util.Functions 
import Numeric.GSL.Integration

-- | top quark pole mass 
mt :: Double 
mt = 173.0 -- 174.3

-- | G_F (in GeV units)
gFermi :: Double
gFermi = 1.166379e-5



abssqr :: (Num a) => Complex a -> a
abssqr (x :+ y) = x*x + y*y 

-- Kinematic functions

beta s = sqrt (1.0 - 4.0 * mt^(2::Int) / s)

p1p3 s z = s/4.0*(1.0 - beta s * z)

p2p3 s z = s/4.0*(1.0 + beta s * z)

fI :: Double -> Complex Double
fI s = (log ( ((1.0+b):+0) / ((1.0-b):+0)) - (0:+pi) )^(2::Int)
  where b = beta s

fP :: Double -> Complex Double
fP s = - ((mt^(2::Int) / s) :+ 0) * fI s

-- | parton-level differential cross section of QCD
--   reference : Dicus, Stange and Willenbrock (hep-ph/9404359)
dsigmaQCD_dz alphaS s z = 
    c1 * beta s * ( ssqr / (p1p3'*p2p3') -  9.0 ) * ( p1p3'^(2::Int) / ssqr + p2p3'^(2::Int) / ssqr + mt^(2::Int)/s - mt^(4::Int) / (4.0*p1p3'*p2p3') )
  where c1  = pi * alphaS^(2::Int) / (12.0 * s) 
        ssqr = s^(2::Int)
        p1p3' = p1p3 s z
        p2p3' = p2p3 s z

-- | parton-level differential cross section of pseudoscalar higgs A
--   reference : Dicus, Stange and Willenbrock (hep-ph/9404359)
dsigmaA_dz mA gammaA alphaS s z = 
    c1 * beta s * abssqr ( fP s * prop )
    - c2 * beta s * (1.0/(p1p3 s z) + 1.0/(p2p3 s z)) * realPart ( fP s * prop )
    + dsigmaQCD_dz alphaS s z
  where c1 = 3.0 * alphaS^(2::Int) * gFermi^(2::Int) * mt^(2::Int) * s^(2::Int) / (2048.0 * pi^3) 
        c2 = alphaS^(2::Int)*gFermi*mt^(2::Int)*s / (256.0*pi*sqrt 2.0)
        prop = 1.0 / ((s - mA^(2::Int)) :+ (mA*gammaA))


-- total cross section ( need to integrate over z = cos theta)
sigmaA mA gammaA alphaS s = integrateQAGS 1e-9 1000 (dsigmaA_dz mA gammaA alphaS s) (-1.0) (1.0)

sigmaQCD alphaS s = integrateQAGS 1e-9 1000 (dsigmaQCD_dz alphaS s) (-1.0) (1.0)
 
