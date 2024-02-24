module Donuts.Internal.Api
  ( not
  , lift
  , newMutVar
  , evalMutVarState
  , setMutVar
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

evalMutVarState :: Monad m => StateT v m a -> v -> m a
evalMutVarState st !v = evalStateT st v

setMutVar :: Monad m => v -> StateT v m ()
setMutVar !v = put v

getMutVar :: Monad m => StateT v m v
getMutVar = get

not :: Bool -> Bool
not = Prelude.not
