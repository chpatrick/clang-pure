{-# LANGUAGE TypeFamilies #-}

module Clang.Refs
  ( Ref(deref), RefType, uderef, Finalizer
  , Root(), root
  , Child(), child
  ) where

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Foreign hiding (void, newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import System.IO.Unsafe (unsafePerformIO)

-- A reference-counted foreign reference. It can be derefenced (like ForeignPtr) and
-- sub-references can be created, ensuring that the finalizer is not called before
-- the sub-references get collected.
type family RefType r
class Ref r where
  incCount :: r -> STM ()
  decCount :: r -> STM ()
  deref :: r -> (Ptr (RefType r) -> IO b) -> IO b

type RefCount = TVar Int
data Node a = Node (ForeignPtr a) RefCount
newtype Root a = Root { rootNode :: Node a }

type Finalizer = IO ()

root :: Finalizer -> Ptr a -> IO (Root a)
root ffin ptr = do 
  refs <- newTVarIO 0
  let fin = void $ forkIO $ do
        ensureZero refs
        ffin
  fptr <- newForeignPtr ptr fin
  return $ Root $ Node fptr refs

ensureZero :: RefCount -> IO ()
ensureZero rc = atomically $ do
  count <- readTVar rc
  guard (count == 0)

incCountNode :: Node a -> STM ()
incCountNode (Node _ rc) = modifyTVar rc succ

decCountNode :: Node a -> STM ()
decCountNode (Node _ rc) = modifyTVar rc pred

derefNode :: Node a -> (Ptr a -> IO b) -> IO b
derefNode (Node fp _) = withForeignPtr fp

type instance RefType (Root a) = a
instance Ref (Root a) where
  incCount = incCountNode . rootNode
  decCount = decCountNode . rootNode
  deref = derefNode . rootNode

data Child p a = Child { parent :: p, childNode :: Node a }

type instance RefType (Child p a) = a
instance Ref p => Ref (Child p a) where
  incCount = incCountNode . childNode
  decCount = decCountNode . childNode
  deref c f
    = deref (parent c) $ \_ ->
      derefNode (childNode c) f

child :: Ref p => p -> (Ptr (RefType p) -> IO ( Finalizer, Ptr a )) -> IO (Child p a)
child p f =
  deref p $ \pptr -> do
    ( ffin, ptr ) <- f pptr
    refs <- newTVarIO 0
    let fin = void $ forkIO $ do
          ensureZero refs
          ffin
          atomically $ decCount p
    atomically $ incCount p
    fptr <- newForeignPtr ptr fin
    return $ Child p $ Node fptr refs

uderef :: Ref r => r -> (Ptr (RefType r) -> IO b) -> b
uderef r f = unsafePerformIO $ deref r f