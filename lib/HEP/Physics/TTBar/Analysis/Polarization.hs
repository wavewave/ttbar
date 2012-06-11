module HEP.Physics.TTBar.Analysis.Polarization where

import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit.Util.Control

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Formatter 
import HEP.Parser.LHEParser.Parser.Conduit
import HEP.Parser.LHEParser.DecayTop

import qualified Data.Text.IO as TIO

import System.IO

import Text.XML.Conduit.Parse.Util

-- import HEP.Automation.MadGraph.LHESanitizer.Parse


import Data.List 

printEvent :: (MonadIO m ) => C.Sink (Maybe LHEvent) m () 
printEvent = do 
  elm <- CL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just ev -> liftIO $ putStrLn (formatLHEvent ev)

printDecayTop :: (MonadIO m )  => C.Sink (Maybe LHEvent) m () 
printDecayTop = do 
  elm <- CL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just ev -> do 
          let (_,_,dtops) = getDecayTop ev
          liftIO $ putStrLn $ intercalate "\n" $ map show dtops  

