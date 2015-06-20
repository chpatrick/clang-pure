module Clang.Refs
  ( Finalizer
  , Node(..)
  , Root(), root
  , Child(), child, parent
  , Ref(deref, node), RefType, ParentType
  , uderef, uderef2
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent
import System.IO.Unsafe

type Finalizer = IO ()

data NodeState = NodeState
  { _collected :: !Bool
  , _refCount :: !Int
  }

makeLenses ''NodeState

data Node a = Node
  { nodePtr :: ForeignPtr a
  , nodeState :: MVar NodeState
  , trueFinalizer :: Finalizer
  }

instance Eq (Node a) where
  n == n' = nodePtr n == nodePtr n'

instance Ord (Node a) where
  n `compare` n' = nodePtr n `compare` nodePtr n'

newtype Root a = Root (Node a)
  deriving (Eq, Ord)

data Child p a = Child p (Node a)

instance Eq (Child p a) where
  Child _ n == Child _ n' = n == n'

instance Ord (Child p a) where
  Child _ n `compare` Child _ n' = n `compare` n'

root :: Finalizer -> Ptr a -> IO (Root a)
root trueFinalizer ptr = do
  nodeState <- newMVar $ NodeState False 0
  nodePtr <- newForeignPtr ptr $
    modifyMVar_ nodeState $ \ns -> do
      when (view refCount ns == 0) trueFinalizer
      return $ ns & collected .~ True
  return $ Root $ Node nodePtr nodeState trueFinalizer

child :: Ref p => p -> (Ptr (RefType p) -> IO ( Finalizer, Ptr a )) -> IO (Child p a)
child p f =
  deref p $ \pptr -> do
    ( fin, ptr ) <- f pptr
    let parentNode = node p
    childNodeState <- newMVar $ NodeState False 0
    modifyMVar_ (nodeState parentNode) (return . (refCount +~ 1))
    nodePtr <- newForeignPtr ptr $
      modifyMVar_ childNodeState $ \ns -> do
        -- No sub-references, finalize immediately. Otherwise,
        -- finalization will happen when the last sub-reference is finalized.
        when (view refCount ns == 0) $ do
          fin
          (decCount $! nodeState parentNode) $! trueFinalizer parentNode
        return $ ns & collected .~ True
    return $ Child p $ Node
      { nodePtr
      , nodeState = childNodeState
      , trueFinalizer = fin
      }

decCount :: MVar NodeState -> Finalizer -> IO ()
decCount parentState parentFin =
  modifyMVar_ parentState $ \case
    NodeState True 1 -> do
      parentFin
      return $ NodeState True 0
    ns -> return $ ns & refCount -~ 1

type family RefType r
type family ParentType r
class Ref r where
  deref :: r -> (Ptr (RefType r) -> IO a) -> IO a
  node :: r -> Node (RefType r)
  parent :: r -> ParentType r

type instance RefType (Root a) = a
type instance ParentType (Root a) = ()
instance Ref (Root a) where
  deref (Root n) = withForeignPtr $ nodePtr n
  node (Root n) = n
  parent _ = ()

type instance RefType (Child p a) = a
type instance ParentType (Child p a) = p
instance Ref p => Ref (Child p a) where
  deref (Child p n) f =
    deref p $ \_ ->
      withForeignPtr (nodePtr n) f
  node (Child _ n) = n
  parent (Child p _) = p

uderef :: Ref r => r -> (Ptr (RefType r) -> IO a) -> a
uderef r f = unsafePerformIO $ deref r f

uderef2 :: (Ref r, Ref r') => r -> r' -> (Ptr (RefType r) -> Ptr (RefType r') -> IO a) -> a
uderef2 r r' f =
  unsafePerformIO $
    deref r $ \p ->
      deref r' $ \p' ->
        f p p'