{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Physics.TTBar.Tester.XSecTester.Common where

import HEP.LHAPDF

lhc7cm  = 7000.0 
lhc14cm = 14000.0 
tevcm   = 1960.0 



initPDFgetAlphaS pdfdataset mu = do 
  initPDFSet pdfdataset
  n <- numberPDF
  mapM_ initPDF [0..n]
  alphasPDF 200.0
