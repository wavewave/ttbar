module HEP.Physics.TTBar.Analysis.TopPairParton where

import HEP.Util.Functions
import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.IORef


-- | 

type DecayTopSink a b m c = Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m c

-- |

data MTopPair = MTopPair { mtopmom :: Maybe FourMomentum
                         , mantitopmom :: Maybe FourMomentum } 


-- | 

data TopPair = TopPair { topmom :: FourMomentum
                       , antitopmom :: FourMomentum }


-- | 

data TopOrAnti = Top FourMomentum | AntiTop FourMomentum


-- | 

ifTopOrAntiTopThenGetMomentum :: DecayTop PtlIDInfo -> MTopPair -> MTopPair 
ifTopOrAntiTopThenGetMomentum (Terminal (PIDInfo 6 pinfo)) mtop = 
  mtop { mtopmom = Just . pupTo4mom . pup $ pinfo } 
ifTopOrAntiTopThenGetMomentum (Terminal (PIDInfo (-6) pinfo)) mtop = 
  mtop { mantitopmom = Just . pupTo4mom . pup $ pinfo }
ifTopOrAntiTopThenGetMomentum (Decay (PIDInfo 6 pinfo,_)) mtop =
  mtop { mtopmom = Just . pupTo4mom . pup $ pinfo } 
ifTopOrAntiTopThenGetMomentum (Decay (PIDInfo (-6) pinfo,_)) mtop =
  mtop { mantitopmom = Just . pupTo4mom . pup $ pinfo } 
ifTopOrAntiTopThenGetMomentum _ mtop = mtop 


-- | 

identifyTopPair :: [DecayTop PtlIDInfo] -> Maybe TopPair
identifyTopPair lst = 
  let result = foldr ifTopOrAntiTopThenGetMomentum (MTopPair Nothing Nothing) lst
  in case (mtopmom result, mantitopmom result) of 
       (Just tmom, Just atmom) -> Just (TopPair tmom atmom)
       _ -> Nothing 


-- | 

checkTTBarEvent :: Maybe (a,b,[DecayTop PtlIDInfo]) -> Maybe TopPair
checkTTBarEvent el = do
  do (_,_,dtops) <- el
     identifyTopPair dtops


-- | 

isForward :: TopPair -> Bool 
isForward (TopPair t at) = let (_,etat,_) = mom_2_pt_eta_phi t
                               (_,etaat,_) = mom_2_pt_eta_phi at
                           in etat > etaat



-- | 

checkTTBarAnd :: (Monad m) => 
                     (c -> TopPair -> c) -> c -> DecayTopSink a b m c
checkTTBarAnd action = CL.fold (\b a -> maybe b (action b) (checkTTBarEvent a))

-- | 

checkTTBarAndM :: (Monad m) => 
                 (c -> TopPair -> m c) -> c -> DecayTopSink a b m c
checkTTBarAndM action = CL.foldM (\b a -> maybe (return b) (action b) (checkTTBarEvent a)) 


-- | 

checkTTBarAndM_ :: (Monad m) => 
                        (TopPair -> m ()) -> DecayTopSink a b m ()
checkTTBarAndM_ action = checkTTBarAndM (\() t -> action t) () 




-- | 

shoutTTBar :: (MonadIO m) => DecayTopSink a b m ()
shoutTTBar = checkTTBarAndM_ (const (liftIO (putStrLn "one ttbar event")))

-- | 

countTTBar :: (MonadIO m) => DecayTopSink a b m Int 
countTTBar = checkTTBarAnd (\x _ ->x+1) 0 

-- |

countFBTTBar :: (MonadIO m) => DecayTopSink a b m (Int,Int)
countFBTTBar = checkTTBarAnd countFB (0,0)
  where countFB (x,y) tpair = if isForward tpair then (x+1,y) else (x,y+1)

-- | 

data FBHL = FBHL { high :: (Int,Int), low :: (Int,Int) } 
            deriving (Show)


countFBHLTTBar :: (MonadIO m) => DecayTopSink a b m FBHL 
countFBHLTTBar = checkTTBarAnd countFBHL FBHL { high = (0,0), low = (0,0) } 
  where countFBHL (FBHL (hf,hb) (lf,lb)) tpair@(TopPair t at)  
          | isForward tpair && invmass t at > 450 = FBHL (hf+1,hb) (lf,lb)
          | isForward tpair && invmass t at <= 450 = FBHL (hf,hb) (lf+1,lb)
          | (not.isForward) tpair && invmass t at > 450 = FBHL (hf,hb+1) (lf,lb)
          | otherwise = FBHL (hf,hb) (lf,lb+1)

-- | 

afbTTBar :: (MonadIO m) => DecayTopSink a b m Double
afbTTBar = countFBTTBar >>= \(f,b) -> let fdbl = fromIntegral f 
                                          bdbl = fromIntegral b
                                      in  return ((fdbl-bdbl) / (fdbl+bdbl))

-- | deprecated 

proceedOneEvent :: (Monad m) => (a -> Sink a m ()) -> Sink a m ()
proceedOneEvent action = CL.head >>= maybe (return ()) (\x -> action x >> proceedOneEvent action)


-- | deprecated

proceedWithActionForTopPair :: (Monad m) => 
                               (TopPair -> Maybe (Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m ())) 
                            -> Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
proceedWithActionForTopPair action = proceedOneEvent $ maybe (return ()) id 
                                                      . (action <=< checkTTBarEvent) 

-- | deprecated

showNonTTBarEvent :: (MonadIO m, Show a) => DecayTopSink a b m ()
showNonTTBarEvent = 
    proceedOneEvent $ 
      \el -> case el of 
               Nothing -> liftIO $ putStrLn "Nothing?"
               Just (a,_,dtops) -> do 
                 case identifyTopPair dtops of 
                   Just _ -> return () 
                   Nothing -> do liftIO $ print a 
                                 mapM_ (\x -> do liftIO . print . fmap pdgid $ x ; liftIO $ putStrLn "." ) dtops 


-- | deprecated

showTTBarEvent :: (MonadIO m) => Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
showTTBarEvent = proceedWithActionForTopPair (const (return . liftIO . putStrLn $ "one ttbar event"))

-- | deprecated 

countTTBarEventUsingIORef :: (MonadIO m) => IORef Int -> DecayTopSink a b m ()
countTTBarEventUsingIORef ref = proceedWithActionForTopPair (const (return . liftIO $ modifyIORef ref (\x->x+1)))


-- | deprecated 

countTTBarFBUsingIORef :: (MonadIO m) => IORef (Int,Int) -> DecayTopSink a b m () 
countTTBarFBUsingIORef ref = proceedWithActionForTopPair f 
   where f x = if isForward x 
                 then return . liftIO $ modifyIORef ref (\(x,y) -> (x+1,y))
                 else return . liftIO $ modifyIORef ref (\(x,y) -> (x,y+1))




