{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

module HEP.Physics.TTBar.FileWork where

import System.IO
import System.Posix.Files 


import Data.Iteratee as Iter
import Data.Iteratee.Util
import Data.Iteratee.ListLike as LL 

import "mtl" Control.Monad.State

import Control.Monad.IO.Class

import LHCOAnalysis 


type JnEnumeratee sFrom sTo m a = Iteratee sTo m a -> Iteratee sFrom m a
type CountIO = StateT Int IO


{- 
-- | Open a binary file and apply cut and do designated action with showing progress
binaryWorkDefault :: (PhyEventClassified -> IO ()) -> JnEnumeratee [PhyEventClassified] [PhyEventClassified] CountIO () -> FilePath -> IO () 
binaryWorkDefault action cutset fn = binaryWorkWithIterTransformer action id id cutset fn 




-- | Open a binary file and apply cut and do designated action with showing progress with before cut and after cut 
binaryWorkWithIterTransformer :: (PhyEventClassified -> IO ()) 
                                 -> (Iteratee [PhyEventClassified] CountIO () -> Iteratee [PhyEventClassified] CountIO ())    -- | before cut 
                                 -> (Iteratee [PhyEventClassified] CountIO () -> Iteratee [PhyEventClassified] CountIO ())    -- | after cut
                                 -> JnEnumeratee [PhyEventClassified] [PhyEventClassified] CountIO () -> FilePath -> IO () 
binaryWorkWithIterTransformer action beforeiter afteriter cutset fn = do   
  inh <- openFile fn ReadMode
  eventlst <- readbyte inh
  (runIterWithState 1) . (splice100 eventlst) . workWithCounter .  beforeiter . cutset . afteriter $ (actionIO action)
  hClose inh
  return ()
-} 

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
