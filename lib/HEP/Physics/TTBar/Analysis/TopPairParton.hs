module HEP.Physics.TTBar.Analysis.TopPairParton where

import HEP.Util.Functions
import HEP.Parser.LHEParser.Type
import Control.Monad.Trans
import Data.Enumerator
import qualified Data.Enumerator.List as EL

data MTopPair = MTopPair { mtopmom :: Maybe FourMomentum
                         , mantitopmom :: Maybe FourMomentum } 

data TopPair = TopPair { topmom :: FourMomentum
                       , antitopmom :: FourMomentum }


data TopOrAnti = Top FourMomentum | AntiTop FourMomentum

ifTopOrAntiTopThenGetMomentum :: DecayTop PtlIDInfo -> MTopPair -> MTopPair 
ifTopOrAntiTopThenGetMomentum (Terminal (PIDInfo 6 pinfo)) mtop = 
  mtop { mtopmom = Just . pupTo4mom . pup $ pinfo } 
ifTopOrAntiTopThenGetMomentum (Terminal (PIDInfo (-6) pinfo)) mtop = 
  mtop { mantitopmom = Just . pupTo4mom . pup $ pinfo }
ifTopOrAntiTopThenGetMomentum _ mtop = mtop 

identifyTopPair :: [DecayTop PtlIDInfo] -> Maybe TopPair
identifyTopPair lst = 
  let result = foldr ifTopOrAntiTopThenGetMomentum (MTopPair Nothing Nothing) lst
  in case (mtopmom result, mantitopmom result) of 
       (Just tmom, Just atmom) -> Just (TopPair tmom atmom)
       _ -> Nothing 

checkTTBarEvent :: Maybe (Maybe (a,b,[DecayTop PtlIDInfo])) -> Maybe TopPair
checkTTBarEvent mel = do
  do mdetail <- mel 
     (_,_,dtops) <- mdetail
     (identifyTopPair dtops)


proceedWithActionForTopPair :: (Monad m) => 
                               (TopPair -> Maybe (Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m ())) 
                            -> Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
proceedWithActionForTopPair action = do 
  mel <- EL.head
  maybe (return ()) (\x -> x >> proceedWithActionForTopPair action)
        $ checkTTBarEvent mel >>= action 


showTTBarEvent :: (MonadIO m) => Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
showTTBarEvent = proceedWithActionForTopPair (const (return . liftIO . putStrLn $ "one ttbar event"))

{-
do 
  mel <- EL.head
  maybe (return ()) (\x -> x >> showTTBarEvent)
        $ checkTTBarEvent mel >> (return . liftIO . putStrLn $ "one ttbar event") -}



 {-
  maybe (return ()) 
        (\(_,_,dtops)-> do case identifyTopPair dtops of 
                             Nothing -> return () 
                             Just _ ->  liftIO . putStrLn $ "one ttbar event" 
                           showTTBarEvent )
        mel
-}
