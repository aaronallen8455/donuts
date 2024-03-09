module Donuts.Internal.Api
  ( not
  , lift
  , newMutVar
  , newMutVarStrict
  , evalMutVarState
  , evalMutVarStateStrict
  , setMutVar
  , setMutVarStrict
  , getMutVar
  , earlyReturnWrapDo
  , LoopControl(..)
  ) where

import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Prelude hiding (not)
import qualified Prelude

earlyReturnWrapDo :: Monad m => ExceptT a m a -> m a
earlyReturnWrapDo = fmap (either id id) . runExceptT

data LoopControl
  = Break
  | Continue

lift :: (MT.MonadTrans t, Monad m) => m a -> t m a
lift = MT.lift

newMutVar :: Monad m => v -> StateT v m a -> m a
newMutVar = flip evalMutVarState

newMutVarStrict :: Monad m => v -> StateT v m a -> m a
newMutVarStrict !v st = evalMutVarState st v

evalMutVarState :: Monad m => StateT v m a -> v -> m a
evalMutVarState = evalStateT

evalMutVarStateStrict :: Monad m => StateT v m a -> v -> m a
evalMutVarStateStrict st !v = evalStateT st v

setMutVar :: Monad m => v -> StateT v m ()
setMutVar = put

setMutVarStrict :: Monad m => v -> StateT v m ()
setMutVarStrict !v = put v

getMutVar :: Monad m => StateT v m v
getMutVar = get

not :: Bool -> Bool
not = Prelude.not
