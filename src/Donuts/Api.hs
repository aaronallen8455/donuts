module Donuts.Api
  ( earlyReturn
  , earlyReturnWrapDo
  , forL
  , whileL
  , repeatL
  , continueL
  , breakL
  , LoopControl(..)
  , lift
  , void
  , when
  , MutAssign(..)
  , Mut(..)
  , newMutVar
  , setMutVar
  , getMutVar
  , not
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Functor as F
import           Prelude hiding (not)
import qualified Prelude

earlyReturn :: Monad m => a -> ExceptT a m b
earlyReturn = throwE

earlyReturnWrapDo :: Monad m => ExceptT a m a -> m a
earlyReturnWrapDo = fmap (either id id) . runExceptT

data LoopControl
  = Break
  | Continue

forL
  :: (Monad m, Foldable f)
  => f a
  -> (a -> ExceptT LoopControl m ())
  -> m ()
forL fa f = foldr go (pure ()) fa
  where
  go x acc = do
    runExceptT (f x) >>= \case
      Right () -> acc
      Left Continue -> acc
      Left Break -> pure ()
  {-# INLINE go #-}

whileL
  :: Monad m
  => Bool
  -> ExceptT LoopControl m ()
  -> m ()
whileL _pred _body =
  -- Rewrite this to
  -- repeatL $ do
  --   when (not pred) breakL
  --   ...
  pure ()

repeatL
  :: Monad m
  => ExceptT LoopControl m ()
  -> m ()
repeatL f =
  let go = runExceptT f >>= \case
        Right () -> go
        Left Continue -> go
        Left Break -> pure ()
   in go

continueL :: Monad m => ExceptT LoopControl m a
continueL = throwE Continue

breakL :: Monad m => ExceptT LoopControl m a
breakL = throwE Break

when :: Monad f => Bool -> f () -> f ()
when = M.when

-- redefined so that the name is available to the plugin even if mtl is not a dependency.
lift :: (MT.MonadTrans t, Monad m) => m a -> t m a
lift = MT.lift

void :: Functor f => f a -> f ()
void = F.void

infixl 0 :=
data MutAssign a b =
  a := b

newtype Mut a = Mut a

newMutVar :: Monad m => v -> StateT v m a -> m a
newMutVar v = (`evalStateT` v)

setMutVar :: Monad m => v -> StateT v m ()
setMutVar = put

getMutVar :: Monad m => StateT v m v
getMutVar = get

not :: Bool -> Bool
not = Prelude.not
