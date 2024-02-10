module Sugar.Api
  ( earlyReturn
  , earlyReturnWrapDo
  , forLoop
  , continueL
  , breakL
  , LoopControl(..)
  , lift
  , void
  , when
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.Except
import qualified Data.Functor as F

earlyReturn :: Monad m => a -> ExceptT a m b
earlyReturn = throwE

earlyReturnWrapDo :: Monad m => ExceptT a m a -> m a
earlyReturnWrapDo = fmap (either id id) . runExceptT

data LoopControl
  = Break
  | Continue

forLoop
  :: (Monad m, Foldable f)
  => f a
  -> (a -> ExceptT LoopControl m ())
  -> m ()
forLoop fa f = foldr go (pure ()) fa
  where
  go x acc = do
    runExceptT (f x) >>= \case
      Right () -> acc
      Left Continue -> acc
      Left Break -> pure ()

continueL :: Monad m => ExceptT LoopControl m a
continueL = throwE Continue

breakL :: Monad m => ExceptT LoopControl m a
breakL = throwE Break

-- redefined so that the name is available to the plugin even if mtl is not a dependency.
lift :: (MT.MonadTrans t, Monad m) => m a -> t m a
lift = MT.lift

void :: Functor f => f a -> f ()
void = F.void

-- TODO should be able to utilize the existing when function.
when :: Monad f => Bool -> f () -> f ()
when = M.when
