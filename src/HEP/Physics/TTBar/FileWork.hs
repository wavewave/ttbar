{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

module HEP.Physics.TTBar.FileWork where

import System.IO

import Data.Iteratee as Iter
import Data.Iteratee.Util

import Control.Monad.State

import LHCOAnalysis 

type JnEnumeratee sFrom sTo m a = Iteratee sTo m a -> Iteratee sFrom m a
type CountIO = StateT Int IO

binaryWorkWithIterTransformer :: (Iteratee [PhyEventClassified] CountIO a) 
                                 -> (Iteratee [PhyEventClassified] CountIO a -> Iteratee [PhyEventClassified] CountIO a )    -- ^ before cut 
                                 -> (Iteratee [PhyEventClassified] CountIO a -> Iteratee [PhyEventClassified] CountIO a)    -- ^ after cut
                                 -> JnEnumeratee [PhyEventClassified] [PhyEventClassified] CountIO a 
                                 -> FilePath 
                                 -> IO a
binaryWorkWithIterTransformer action beforeiter afteriter cutset fn = do   
  inh <- openFile fn ReadMode
  eventlst <- readbyte inh
  (x,()) <- (runIterWithState 1) . (splice100 eventlst) . workWithCounter .  beforeiter . cutset . afteriter $ action
  hClose inh
  return x


-- //////////////////////////////////////////////////////////////////////// --

-- | New Interface. Combine arguments of binaryWorkWithIterTransformer into 
--   a single structure 

data BinaryWorkSpec a = BWSpec { 
  main_process    :: Iteratee [PhyEventClassified] CountIO a,  -- (PhyEventClassified -> IO ()), 
  precut_process  :: Iteratee [PhyEventClassified] CountIO a -> Iteratee [PhyEventClassified] CountIO a, 
  postcut_process :: Iteratee [PhyEventClassified] CountIO a -> Iteratee [PhyEventClassified] CountIO a, 
  cut_process     ::  Iteratee [PhyEventClassified] CountIO a -> Iteratee [PhyEventClassified] CountIO a 
  }


-- | Calling function for binaryWorkWithIterTransformer

binaryWorkWithSpec :: BinaryWorkSpec a -> FilePath -> IO a
binaryWorkWithSpec spec fn = 
  binaryWorkWithIterTransformer (main_process spec)
                                (precut_process spec)
                                (postcut_process spec)
                                (cut_process spec)
                                fn
