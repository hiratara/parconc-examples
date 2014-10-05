module Main (main) where
import Control.Concurrent
import qualified Control.Concurrent.STM as STM

main :: IO ()
main = do
  done <- STM.atomically newEmptyTMVar
  _ <- forkIO $ do
    threadDelay 100000
    STM.atomically $ putTMVar done ()
    STM.atomically $ putTMVar done ()
    STM.atomically $ putTMVar done ()
  STM.atomically $ takeTMVar done
  threadDelay 200000
  STM.atomically $ takeTMVar done
  STM.atomically $ takeTMVar done
  putStrLn "Hello World"

-- <<TMVar
newtype TMVar a = TMVar (STM.TVar (Maybe a))
-- >>

newTMVar :: a -> STM.STM (TMVar a)
newTMVar a = do
  t <- STM.newTVar (Just a)
  return (TMVar t)

-- <<newEmptyTMVar
newEmptyTMVar :: STM.STM (TMVar a)
newEmptyTMVar = do
  t <- STM.newTVar Nothing
  return (TMVar t)
-- >>

-- <<takeTMVar
takeTMVar :: TMVar a -> STM.STM a
takeTMVar (TMVar t) = do
  m <- STM.readTVar t                       -- <1>
  case m of
    Nothing -> STM.retry                    -- <2>
    Just a  -> do
      STM.writeTVar t Nothing               -- <3>
      return a
-- >>

-- <<putTMVar
putTMVar :: TMVar a -> a -> STM.STM ()
putTMVar (TMVar t) a = do
  m <- STM.readTVar t
  case m of
    Nothing -> do
      STM.writeTVar t (Just a)
      return ()
    Just _  -> STM.retry
-- >>

-- <<takeEitherTMVar
takeEitherTMVar :: TMVar a -> TMVar b -> STM.STM (Either a b)
takeEitherTMVar ma mb =
  fmap Left (takeTMVar ma)
    `STM.orElse`
  fmap Right (takeTMVar mb)
-- >>
