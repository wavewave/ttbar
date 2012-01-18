module HEP.Physics.TTBar.Analysis.TopPairParton where

import HEP.Util.Functions
import HEP.Parser.LHEParser.Type
import Control.Monad
import Control.Monad.Trans
import Data.Enumerator
import qualified Data.Enumerator.List as EL

import Data.IORef

type DecayTopIteratee a b m = Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m 

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

checkTTBarEvent :: Maybe (a,b,[DecayTop PtlIDInfo]) -> Maybe TopPair
checkTTBarEvent el = do
  do (_,_,dtops) <- el
     identifyTopPair dtops


proceedOneEvent :: (Monad m) => (a -> Iteratee a m ()) -> Iteratee a m ()
proceedOneEvent action = EL.head >>= maybe (return ()) (\x -> action x >> proceedOneEvent action)


proceedWithActionForTopPair :: (Monad m) => 
                               (TopPair -> Maybe (Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m ())) 
                            -> Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
proceedWithActionForTopPair action = proceedOneEvent $ maybe (return ()) id 
                                                       . (action <=< checkTTBarEvent) 
--  proceedOneEvent $ (\x -> x >> proceedWithActionForTopPair action)
--  


{-
showNonTTBarEvent :: (MonadIO m) => DecayTopIteratee a b m ()
showNonTTBarEvent = 
    proceedOneEvent $ \el -> do (_,_,dtops) <- el 
                                case identifyTopPair dtops of 
                                  Just _ -> return () 
                                  Nothing -> return () --  mapM_ (liftIO . print . fmap pdgid ) dtops 
-}

{- do 
  mel <- EL.head
  maybe (return ()) (\x -> x >> showNonTTBarEvent)
        $ do mdetail <- mel 
             (_,_,dtops) <- mdetail 
             case identifyTopPair dtops of 
               Just _ -> return (return () )
               Nothing -> return (mapM_ (liftIO . print . fmap pdgid ) dtops )
-}


showTTBarEvent :: (MonadIO m) => Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
showTTBarEvent = proceedWithActionForTopPair (const (return . liftIO . putStrLn $ "one ttbar event"))

countTTBarEventUsingIORef :: (MonadIO m) => IORef Int -> DecayTopIteratee a b m ()
countTTBarEventUsingIORef ref = proceedWithActionForTopPair (const (return . liftIO $ modifyIORef ref (\x->x+1)))

