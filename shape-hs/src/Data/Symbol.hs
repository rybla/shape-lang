module Data.Symbol (newSymbol, Symbol) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype Symbol = Symbol Int deriving (Eq, Ord, Show)

{-# NOINLINE symbolCountSource #-}
symbolCountSource :: IORef Int
symbolCountSource = unsafePerformIO (newIORef 0)

newSymbol :: () -> Symbol
newSymbol _ = unsafePerformIO $ do
  i <- atomicModifyIORef' symbolCountSource $ \i -> (i + 1, i)
  return (Symbol i)
