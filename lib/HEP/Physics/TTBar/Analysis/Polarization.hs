module HEP.Physics.TTBar.Analysis.Polarization where

import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.Enumerator as E
import Data.Enumerator.Util

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Formatter 
import HEP.Parser.LHEParser.Parser.Enumerator
import HEP.Parser.LHEParser.DecayTop

import qualified Data.Text.IO as TIO

import System.IO

import Text.XML.Enumerator.Parse.Util

-- import HEP.Automation.MadGraph.LHESanitizer.Parse

import qualified Data.Enumerator.List as EL

import Data.List 

printEvent :: (MonadIO m ) => E.Iteratee (Maybe LHEvent) m () 
printEvent = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just ev -> liftIO $ putStrLn (formatLHEvent ev)

printDecayTop :: (MonadIO m )  => E.Iteratee (Maybe LHEvent) m () 
printDecayTop = do 
  elm <- EL.head 
  case elm of 
    Nothing -> return () 
    Just maybec ->   
      case maybec of 
        Nothing -> return () 
        Just ev -> do 
          let (_,_,dtops) = getDecayTop ev
          liftIO $ putStrLn $ intercalate "\n" $ map show dtops  

