{-# OPTIONS -XFlexibleInstances #-}
module L2Stat where

import Controller
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import qualified Data.DList as DL
import Types
import L2

start = (LightningVM t0 mem0)

data LightningVM = LightningVM Time Z

instance Tick LightningVM where
    tick inp = do (LightningVM t z)  <- get
                  let (z', outp) = f z inp
                  put (LightningVM (t+1) z')
                  tell $! DL.singleton $ Trace1 t inp outp 
                  return outp
    getTime (LightningVM t z) = t
