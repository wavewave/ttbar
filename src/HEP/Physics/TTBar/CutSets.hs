{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}

module HEP.Physics.TTBar.CutSets where
  
import Data.Iteratee as Iter 
  
import HEP.Physics.TTBar.Cuts
import Data.Iteratee.Util

import LHCOAnalysis


import "mtl" Control.Monad.State

-- | Cut Combining Combinator 

combineCutList :: (Monad m) => [PhyEventClassified -> Bool] -> Iter.Iteratee [PhyEventClassified] m a 
                  -> Iter.Iteratee [PhyEventClassified] m a
combineCutList cset iter = jn (filtre (checkall_cuts cset) iter)

cuts :: (Monad m) => [PhyEventClassified -> Bool] -> Iter.Iteratee [PhyEventClassified] m a 
        -> Iter.Iteratee [PhyEventClassified] m a
cuts = combineCutList

-- | Selecting Cut by event number

selectEvent :: (Monad m) => 
               Int -> Iter.Iteratee [PhyEventClassified] m a -> Iter.Iteratee [PhyEventClassified] m a
selectEvent evtnum = jn . filtre (\p -> eventid p == evtnum) 



-- | sequential cut 

sequentialcuttest = count
                <+> cuts [ cut_photon_veto          ] count
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet    ] count
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton        ] count
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton 
                         , (cut_missing_pt 20)      ] count 
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton 
                         , cut_missing_pt 20
                         , cut_lepton_eta_pt 1.1 20 ] count
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton 
                         , cut_missing_pt 20
                         , cut_lepton_eta_pt 1.1 20
                         , cut_n_jet_eta_pt 4 2.0 20
                         ] count 
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton 
                         , cut_missing_pt 20
                         , cut_lepton_eta_pt 1.1 20
                         , cut_n_jet_eta_pt 4 2.0 20
                         , cut_central_bjet_eta 1.0
                         ] count 
                <+> cuts [ cut_photon_veto
                         , cut_morethanzero_bjet
                         , cut_single_lepton 
                         , cut_missing_pt 20
                         , cut_lepton_eta_pt 1.1 20
                         , cut_n_jet_eta_pt 5 2.0 20 -- changed here
                         , cut_central_bjet_eta 1.0
                         ] count 


                <+> count_marker 1000 0 


cutset20101111AsList :: [ PhyEventClassified -> Bool ] 
cutset20101111AsList = [ cut_photon_veto
                       , cut_morethanzero_bjet
                       , cut_single_lepton 
                       , cut_missing_pt 20
                       , cut_lepton_eta_pt 1.1 20
                       , cut_n_jet_eta_pt 4 2.0 20
                       , cut_central_bjet_eta 1.0
                       ]  
cutset20101111 :: (Monad m, MonadIO m) => 
                Iteratee [PhyEventClassified] m a 
                -> Iteratee [PhyEventClassified] m a 
cutset20101111 = combineCutList cutset20101111AsList
