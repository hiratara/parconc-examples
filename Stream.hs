{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}
-- -Wall 

-- A module for stream processing built on top of Control.Monad.Par

-- (In the future may want to look into the stream interface used by
--  the stream fusion framework.)

module Stream
 ( 
   Stream, streamFromList, streamMap, streamFold, streamFilter
 ) where

import Control.Monad.Par.Scheds.Trace as P
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Types

-- <<IList
data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)

type ForkSetting = (Int, Int)

defaultForkSetting :: ForkSetting
defaultForkSetting = (200, 100)

type Stream a = IVar (IList a)
-- >>

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork _ (Cons a b)) = rnf a `seq` rnf b

-- -----------------------------------------------------------------------------
-- Stream operators

-- <<streamFromList
streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList = streamFromList' defaultForkSetting

streamFromList' :: NFData a => ForkSetting -> [a] -> Par (Stream a)
streamFromList' (chunkSize, forkPoint) xs = do
  var <- new                            -- <1>
  fork $ kick xs var                    -- <2>
  return var                            -- <3>
 where
  loop _ [] var _ = put var Nil   -- <4>
  loop (1, _) (x:_) var (Just next) = put var (Cons x next)
  loop (n, 0) (x:xs) var Nothing = do
    tail <- new
    nVar <- new
    put var (Fork (kick (drop (n - 1) xs) nVar)
                  (Cons x tail))
    loop (n - 1, -1) xs tail (Just nVar)
  loop (n, k) (x:xs) var nVar = do      -- <5>
    tail <- new                         -- <6>
    put var (Cons x tail)               -- <7>
    loop (n - 1, k - 1) xs tail nVar    -- <8>
  kick xs var = loop (chunkSize, forkPoint) xs var Nothing
-- >>

-- <<streamMap
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap = streamMap' defaultForkSetting

streamMap' :: NFData b => ForkSetting -> (a -> b) -> Stream a -> Par (Stream b)
streamMap' (chunkSize, forkPoint) fn instrm = do
  outstrm <- new
  fork $ kick instrm outstrm
  return outstrm
 where
  loop (1, _) instrm outstrm (Just next) = do
    ilst <- get instrm
    case ilst of
      Nil -> put outstrm Nil
      Cons h tail -> do
        newtl <- new
        put next (tail, newtl)
        put outstrm (Cons (fn h) newtl)
      Fork kickForking (Cons h tail) -> do
        fork kickForking
        newtl <- new
        put next (tail, newtl)
        put outstrm (Cons (fn h) newtl)
  loop (n, 0) instrm outstrm Nothing = do
    ilst <- get instrm
    case ilst of
      Nil -> put outstrm Nil
      Cons h t -> do
        newtl <- new
        nextstrm <- new
        put outstrm (Fork (kick' nextstrm)
                          (Cons (fn h) newtl))
        loop (n - 1, -1) t newtl (Just nextstrm)
      Fork kickForking (Cons h t) -> do
        fork kickForking
        newtl <- new
        nextstrm <- new
        put outstrm (Fork (kick' nextstrm)
                          (Cons (fn h) newtl))
        loop (n - 1, -1) t newtl (Just nextstrm)
  loop (n, k) instrm outstrm nextstrm = do
    ilst <- get instrm
    case ilst of
      Nil -> put outstrm Nil
      Cons h t -> do
        newtl <- new
        put outstrm (Cons (fn h) newtl)
        loop (n - 1, k - 1) t newtl nextstrm
      Fork kickForking (Cons h t) -> do
        fork kickForking
        newtl <- new
        put outstrm (Cons (fn h) newtl)
        loop (n - 1, k - 1) t newtl nextstrm
  kick instrm outstrm = loop (chunkSize, forkPoint) instrm outstrm Nothing
  kick' var = do
    (instrm, outstrm) <- get var
    kick instrm outstrm
-- >>


-- | Reduce a stream to a single value.  This function will not return
--   until it reaches the end-of-stream.
-- <<streamFold
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  ilst <- get instrm
  case ilst of
    Nil      -> return acc
    Cons h t -> streamFold fn (fn acc h) t
    Fork kick (Cons h t) -> fork kick >> streamFold fn (fn acc h) t
-- >>

streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p instr = do
    outstr <- new
    fork $ loop instr outstr
    return outstr
  where
    loop instr outstr = do
      v <- get instr
      case v of
        Nil -> put outstr Nil
        Cons x instr'
          | p x -> do
             tail <- new
             put_ outstr (Cons x tail)
             loop instr' tail
          | otherwise -> do
             loop instr' outstr

