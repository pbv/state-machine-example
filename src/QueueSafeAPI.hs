{-
  Haskell wrapper over bounded queues implementation
  Thread safe version, using MVars
-}
module QueueSafeAPI(Queue, Size, Elem,
               new, delete,
               enqueue, dequeue,
               is_full, is_empty) where
import Control.Monad (when)
import Control.Concurrent (MVar, newMVar, withMVar)
import Data.Void
import Foreign.C
import Foreign.Ptr

type Queue = MVar QueuePtr
type QueuePtr = Ptr Void

type Size = CSize
type Elem = CInt

foreign import ccall  "new" _new :: Size -> IO QueuePtr
foreign import ccall "delete" _delete :: QueuePtr -> IO ()
foreign import ccall "is_full" _is_full :: QueuePtr -> IO CInt
foreign import ccall "is_empty" _is_empty :: QueuePtr -> IO CInt
foreign import ccall "enqueue" _enqueue :: QueuePtr -> Elem -> IO ()
foreign import ccall "dequeue" _dequeue :: QueuePtr -> IO Elem

new :: Size -> IO Queue
new size = do
  q <- _new size
  newMVar q

delete :: Queue -> IO ()
delete mv = withMVar mv _delete

enqueue :: Queue -> Elem -> IO ()
enqueue mv x = withMVar mv $ \q -> do
  c <- _is_full q
  when (c==0) (_enqueue q x)

dequeue :: Queue -> IO Elem
dequeue mv = withMVar mv $ \q -> do
  c <- _is_empty q
  if c==0 then _dequeue q else return 0 


is_full, is_empty :: Queue -> IO Bool
is_full mv = toBool <$>  withMVar mv _is_full 
is_empty mv = toBool <$> withMVar mv _is_empty 

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> CInt
fromBool False = 0
fromBool True  = 1
