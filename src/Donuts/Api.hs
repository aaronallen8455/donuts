module Donuts.Api
  ( earlyReturn
  , forL
  , whileL
  , repeatL
  , continueL
  , breakL
  , when
  , Mut(..)
  -- * Internal
  , not
  , lift
  , newMutVar
  , evalMutVarState
  , setMutVar
  , getMutVar
  , earlyReturnWrapDo
  , MutAssign(..)
  , LoopControl(..)
  ) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Prelude hiding (not)
import qualified Prelude

-- | Short circuits a monadic computation, returning the given value.
--
-- @
-- echo :: IO Int
-- echo = do
--   repeatL $ do
--     inp <- getLine
--     when (inp == "exit") (earlyReturn 5)
--     putStrLn inp
-- @
--
-- @since 0.1.0.0
earlyReturn :: Monad m => a -> ExceptT a m b
earlyReturn = throwE

earlyReturnWrapDo :: Monad m => ExceptT a m a -> m a
earlyReturnWrapDo = fmap (either id id) . runExceptT

data LoopControl
  = Break
  | Continue

-- | Loop over the contents of a 'Foldable' container.
--
-- @
-- sum :: [Int] -> Int
-- sum xs = runIdentity $ do
--   let Mut r = 0
--   forL xs $ \x -> do
--     r := r + x
--   pure r
-- @
--
-- @since 0.1.0.0
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

-- | A loop that stops when the given predicate evaluates to @False@. Note that
-- evaluation of the predicate expression respects mutability.
--
-- @
-- countdown :: Int -> IO ()
-- countdown n = do
--   let Mut i = n
--   whileL (i > 0) $
--     print i
--     i := i - 1
-- @
--
-- @since 0.1.0.0
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

-- | An indefinitely repeating loop. 'breakL' or 'earlyReturn' can be used to
-- exit the loop.
--
-- @since 0.1.0.0
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

-- | Inside the body of a loop, skip to the next iteration of that loop.
--
-- @
-- skipEven :: IO ()
-- skipEven = do
--   forL [1..20] $ \i ->
--     when (even i) continueL
--     print i
-- @
--
-- @since 0.1.0.0
continueL :: Monad m => ExceptT LoopControl m a
continueL = throwE Continue

-- | Exits a loop early.
--
-- @since 0.1.0.0
breakL :: Monad m => ExceptT LoopControl m a
breakL = throwE Break

-- | Works the same as 'Control.Monad.when' but preserves the mutability of
-- variables and can contain loop controls and 'earlyReturn' statements.
--
-- @since 0.1.0.0
when :: Monad f => Bool -> f () -> f ()
when = M.when

-- redefined so that the name is available to the plugin even if mtl is not a dependency.
lift :: (MT.MonadTrans t, Monad m) => m a -> t m a
lift = MT.lift

infixl 0 :=
data MutAssign a b =
  a := b

newtype Mut a = Mut a

newMutVar :: Monad m => v -> StateT v m a -> m a
newMutVar = flip evalMutVarState

evalMutVarState :: Monad m => StateT v m a -> v -> m a
evalMutVarState = evalStateT

setMutVar :: Monad m => v -> StateT v m ()
setMutVar = put

getMutVar :: Monad m => StateT v m v
getMutVar = get

not :: Bool -> Bool
not = Prelude.not
