{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE OverloadedRecordDot #-}


module QueueTest where
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Test.StateMachine
import           Test.StateMachine.TreeDiff
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Data.Foldable
import           Data.Functor.Classes  (Eq1, Ord1)
import           GHC.Generics  (Generic, Generic1)
import           Foreign.C.Types
import           QueueAPI
-- import           QueueSafeAPI
------------------------------------------------------------------------

type QueueRef = Reference (Opaque Queue) 

data Command r
  = Create Size
  | Delete (QueueRef r)
  | Enqueue (QueueRef r) Elem
  | Dequeue (QueueRef r)
  | IsEmpty (QueueRef r)
  | IsFull  (QueueRef r)
  deriving (Eq, Generic1, Rank2.Functor,
             Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving instance Show (Command Symbolic)
deriving instance Show (Command Concrete)

data Response r
  = Created (QueueRef r)
  | Deleted
  | Enqueued
  | Dequeued Elem
  | Replied Bool
  deriving (Eq,  Generic1, Rank2.Foldable)

deriving instance Show (Response Symbolic)
deriving instance Show (Response Concrete)

-- | Semantics
semantics :: Command Concrete -> IO (Response Concrete)
semantics cmd = case cmd of
  Create size -> Created <$> (reference . Opaque <$> new size)
  Delete ref -> delete (opaque ref) >> pure Deleted
  Enqueue ref val -> enqueue (opaque ref) val >> pure Enqueued
  Dequeue ref -> Dequeued <$> dequeue (opaque ref)
  IsEmpty ref -> Replied <$> is_empty (opaque ref)
  IsFull ref -> Replied <$> is_full (opaque ref)

-- | Model

-- | functional model for several queues
-- assoc list of references to queue models
newtype Model r = Model [(QueueRef r, QueueModel)]
  deriving (Generic, Show)

(!) :: Eq a => [(a,b)] -> a -> b
lst ! key = case lookup key lst of
              Just v -> v
              Nothing -> error "invalid key"


-- | fuctional model for a single queue
data QueueModel = QueueModel { size :: Size, values :: [Elem] }
  deriving (Show, Generic)

instance ToExpr CInt where  toExpr = defaultExprViaShow
instance ToExpr CSize where toExpr = defaultExprViaShow

instance ToExpr QueueModel
instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model []

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition (Model model) cmd = case cmd of
  Create size -> Top
  Delete ref -> ref `member` map fst model .// "Delete"
  Enqueue ref val -> ref `member` map fst model .// "Enqueue"
  Dequeue ref -> ref `member` map fst model .// "Dequeue"
  IsEmpty ref -> ref `member` map fst model .// "IsEmpty"
  IsFull ref -> ref `member` map fst model .// "IsFull"


transition :: Ord1 r => Model r -> Command r -> Response r -> Model r
transition model cmd resp = case (cmd, resp) of
  (Create size, Created ref)   -> createQ size ref model
  (Delete ref, Deleted)        -> deleteQ ref model
  (Enqueue ref val, Enqueued)  -> enqueueQ ref val model
  (Dequeue ref, Dequeued _)    -> dequeueQ ref model
  (IsEmpty ref, Replied _)     -> model
  (IsFull ref, Replied _)      -> model
  _                            -> error "should not have happened"

postcondition ::
  Model Concrete ->
  Command Concrete -> Response Concrete -> Logic
postcondition (Model m) cmd resp = case (cmd, resp) of
  (Create size, Created ref) -> Top
  (Delete ref, Deleted) -> Top
  (Enqueue ref val, Enqueued) -> Top
  (Dequeue ref, Dequeued val) ->
    let queue = m!ref
    in case queue.values of
      [] -> Top 
      (val':_) -> val .== val' .// "Dequeue"
  (IsEmpty ref, Replied b) -> let queue = m!ref
                              in b .== isEmptyQ queue .// "IsEmpty"
  (IsFull ref, Replied b) -> let queue = m!ref
                             in b .== isFullQ queue .// "IsFull"

                               
isFullQ, isEmptyQ :: QueueModel -> Bool
isFullQ queue
  = length queue.values == fromIntegral queue.size
isEmptyQ queue
  = null queue.values


createQ :: Ord1 r => Size -> QueueRef r -> Model r -> Model r
createQ size ref (Model model)
  = let queue = QueueModel { size=size, values=[] }
    in Model ((ref, queue) : model)

deleteQ :: Ord1 r => QueueRef r -> Model r -> Model r
deleteQ ref (Model model) = Model [(r,q) | (r,q) <- model, r /= ref]

enqueueQ ref val (Model model)
  = let q = model!ref 
    in  Model (update q)
  where
    update queue
        | length queue.values < fromIntegral queue.size =
            (ref, queue{values=queue.values ++ [val]}) :
            [(r,q) | (r,q)<-model, r/=ref]
        | otherwise = model

dequeueQ :: Ord1 r => QueueRef r -> Model r -> Model r
dequeueQ ref (Model model)
  = let q = model!ref 
    in Model (update q)
  where
    update queue
      = case queue.values of
          (_:rest) -> 
            (ref, queue{values=rest}):[(r,q) | (r,q)<-model, r/=ref]
          [] ->
            model

          
-----------------------------------------------------------------
-- | Generator and shrinker for commands
-----------------------------------------------------------------
generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator (Model model)
  | null model = Just genCreate
  | otherwise = Just $ frequency $ 
    [ (1, genCreate),
      (4, Enqueue <$> elements refs <*> arbitrary),
      (4, Dequeue <$> elements refs),
      (1, IsEmpty <$> elements refs),
      (1, IsFull <$> elements refs)
    ]
  where
    refs = map fst model

genCreate :: Gen (Command Symbolic)
genCreate = Create <$> choose (1,10)

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ (Create size) = [Create size' | size'<-shrink size, size'>0]
shrinker _ (Enqueue ref value) = [Enqueue ref value' | value' <- shrink value]
shrinker _ _             = []
  
--------------------------------------------------------------
-- | Mock responses
-------------------------------------------------------------
mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model model) cmd = case cmd of
  Create size     -> Created <$> genSym
  Delete ref      -> pure Deleted
  Enqueue ref val -> pure Enqueued
  Dequeue ref     -> 
    let first = case values (model!ref) of
          [] -> 0
          (v:_) -> v
    in Dequeued <$> pure first
  IsEmpty ref     -> Replied <$> pure (null (values (model!ref)))
  IsFull ref      ->  
    let queue = model!ref
    in Replied <$> pure (length queue.values == fromIntegral (queue.size))


------------------------------------------------------------------
-- | Cleanup
------------------------------------------------------------------
cleanup :: Model Concrete -> IO ()
cleanup (Model model) = mapM_ (delete . opaque) (map fst model)


-----------------------------------------------------------------
-- | The state machine
-----------------------------------------------------------------
sm :: StateMachine Model Command IO Response
sm = StateMachine initModel transition precondition postcondition Nothing generator shrinker semantics mock cleanup


prop_sequential :: Property
prop_sequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, _model, res) <- runCommands sm cmds
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))


prop_parallel :: Property
prop_parallel = forAllParallelCommands sm Nothing $ \cmds -> monadicIO $ do
  prettyParallelCommands cmds =<< runParallelCommands sm cmds

  
