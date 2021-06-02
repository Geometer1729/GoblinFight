{-# LANGUAGE
  LambdaCase
  #-}

module MAsync where

import Types
import Control.Concurrent
import Control.Monad.Trans.Free
import Control.Monad.Reader

masync :: IO a -> MAsync a
masync ioa = do
    asyncing <- ask
    if asyncing
       then lift $ async ioa
       else liftIO ioa

async :: IO a -> Async a
async ioa = FreeT $ do
  mvar <- newEmptyMVar
  _ <- forkIO $ ioa >>= putMVar mvar
  return $ Free $ MCF mvar return

tryAwaits :: Async a -> IO (Either a (Async a))
tryAwaits (FreeT input) = do
  input >>= \case
    Pure a -> return (Left a)
    Free (MCF mvar f) -> do
      tryTakeMVar mvar >>= \case
        Nothing -> return $ Right $ FreeT $ return $ Free (MCF mvar f)
        Just x -> tryAwaits (f x)

forceAwaits :: Async a -> IO a
forceAwaits (FreeT input) = do
  input >>= \case
    Pure a -> return a
    Free (MCF mvar f) -> do
      x <- takeMVar mvar
      forceAwaits $ f x
