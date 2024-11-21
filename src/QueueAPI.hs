{-
  Haskell wrapper over bounded queues implementation
-}
module QueueAPI(Queue, Size, Elem,
               new, delete,
               enqueue, dequeue,
               is_full, is_empty) where
import Control.Monad (when)
import Data.Void
import Foreign.C
import Foreign.Ptr

type Queue = Ptr Void -- abstract type
type Size = CSize
type Elem = CInt

foreign import ccall unsafe  "new" new :: Size -> IO Queue
foreign import ccall unsafe "delete" delete :: Queue -> IO ()
foreign import ccall unsafe "is_full" _is_full :: Queue -> IO CInt
foreign import ccall unsafe "is_empty" _is_empty :: Queue -> IO CInt
foreign import ccall unsafe "enqueue" _enqueue :: Queue -> Elem -> IO ()
foreign import ccall unsafe "dequeue" _dequeue :: Queue -> IO Elem

enqueue :: Queue -> Elem -> IO ()
enqueue q x = do
  c <- is_full q
  when (not c) $ _enqueue q x

dequeue :: Queue -> IO Elem
dequeue q = do
  c <- is_empty q
  if c then return 0 else _dequeue q


is_full, is_empty :: Queue -> IO Bool
is_full q = toBool <$>  _is_full q
is_empty q = toBool <$> _is_empty q

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> CInt
fromBool False = 0
fromBool True  = 1
