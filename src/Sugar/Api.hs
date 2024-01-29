module Sugar.Api
  ( earlyReturn
  , earlyReturnWrapDo
  , forLoop
  , lift
  , void
  , when
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.Except
import           Data.Foldable
import qualified Data.Functor as F

earlyReturn :: Monad m => a -> ExceptT a m b
earlyReturn = throwE

earlyReturnWrapDo :: Monad m => ExceptT a m a -> m a
earlyReturnWrapDo = fmap (either id id) . runExceptT

forLoop :: (Monad m, Foldable f) => f a -> (a -> m ()) -> m ()
forLoop = for_

-- redefined so that the name is available to the plugin even if mtl is not a dependency.
lift :: (MT.MonadTrans t, Monad m) => m a -> t m a
lift = MT.lift

void :: Functor f => f a -> f ()
void = F.void

-- TODO should be able to utilize the existing when function.
when :: Applicative f => Bool -> f () -> f ()
when = M.when
