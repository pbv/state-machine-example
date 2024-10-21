{-
  Haskell wrapper over bounded queues implementation
-}
module QueueAPI(Queue, Size, Elem,
               new, delete,
               enqueue, dequeue,
               is_full, is_empty) where

import Data.Void
import Foreign.C
import Foreign.Ptr

type Queue = Ptr Void -- abstract type
type Size = CSize
type Elem = CInt

foreign import ccall unsafe  "new" new :: Size -> IO Queue
foreign import ccall unsafe  "delete" delete :: Queue -> IO ()
foreign import ccall unsafe  "is_full" _is_full :: Queue -> IO Elem
foreign import ccall unsafe  "is_empty" _is_empty :: Queue -> IO Elem
foreign import ccall unsafe  "enqueue" enqueue :: Queue -> Elem -> IO ()
foreign import ccall unsafe  "dequeue" dequeue :: Queue -> IO Elem


is_full, is_empty :: Queue -> IO Bool
is_full q = toBool <$>  _is_full q
is_empty q = toBool <$> _is_empty q

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> CInt
fromBool False = 0
fromBool True  = 1
