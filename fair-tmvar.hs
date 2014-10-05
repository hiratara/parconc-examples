module Main (main) where
import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad (join)

main :: IO ()
main = do
  done <- STM.atomically newEmptyTMVar :: IO (TMVar Int)
  _ <- forkIO $ do
    threadDelay 100000
    join . STM.atomically $ putTMVar done 1
    join . STM.atomically $ putTMVar done 2
    join . STM.atomically $ putTMVar done 3
    join . STM.atomically $ putTMVar done 4
  n1 <- join . STM.atomically $ takeTMVar done
  print n1
  threadDelay 200000
  n2 <- join . STM.atomically $ takeTMVar done
  print n2
  n3 <- join . STM.atomically $ takeTMVar done
  print n3
  n4 <- join . STM.atomically $ takeTMVar done
  print n4
  putStrLn "Hello World"

-- <<TMVar
data TMVar a = TMVar (STM.TVar (Maybe a))
                     (STM.TVar [STM.TVar (Maybe a)])
-- >>

newTMVar :: a -> STM.STM (TMVar a)
newTMVar a = do
  t <- STM.newTVar (Just a)
  queue <- STM.newTVar []
  return (TMVar t queue)

-- <<newEmptyTMVar
newEmptyTMVar :: STM.STM (TMVar a)
newEmptyTMVar = do
  t <- STM.newTVar Nothing
  queue <- STM.newTVar []
  return (TMVar t queue)
-- >>

-- <<takeTMVar
takeTMVar :: TMVar a -> STM.STM (IO a)
takeTMVar (TMVar t queue) = do
  m <- STM.readTVar t                       -- <1>
  qs <- STM.readTVar queue
  case m of
    Nothing -> do
      t' <- STM.newTVar Nothing
      STM.writeTVar queue (qs ++ [t'])
      return $ do
        a <- STM.atomically $ do
          tvar <- STM.readTVar t'
          case tvar of
           Nothing -> STM.retry             -- <2>
           Just a -> return a
        return a
    Just a  -> do
      case qs of
       [] -> do
         STM.writeTVar t Nothing            -- <3>
         return (return a)
       (q:qs') -> do
         nextput <- STM.readTVar q
         STM.writeTVar q Nothing
         STM.writeTVar t nextput
         STM.writeTVar queue qs'
         return (return a)
-- >>

-- <<putTMVar
putTMVar :: Show a => TMVar a -> a -> STM.STM (IO ())
putTMVar (TMVar t queue) a = do
  m <- STM.readTVar t
  qs <- STM.readTVar queue
  case m of
    Nothing -> do
      case qs of
       [] -> do
         STM.writeTVar t (Just a)
         return (return ())
       (q:qs') -> do
         STM.writeTVar q (Just a)
         STM.writeTVar queue qs'
         return (return ())
    Just _  -> do
      t' <- STM.newTVar (Just a)
      STM.writeTVar queue (qs ++ [t'])
      return $ do
        STM.atomically $ do
          tvar <- STM.readTVar t'
          case tvar of
           Nothing -> return ()
           Just _ -> STM.retry
        return ()
-- >>
