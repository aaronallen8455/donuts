module Donuts.Api
  ( earlyReturn
  , forL
  , whileL
  , repeatL
  , continueL
  , breakL
  , when
  , Mut(..)
  , MutAssign(..)
  ) where

import qualified Control.Monad as M
import           Control.Monad.Trans.Except

import           Donuts.Internal.Api (LoopControl(..))

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

-- | Used to declare a mutable variable. Works with both let statements and
-- monadic binds.
--
-- @
-- do
-- let Mut x = True
-- Mut y <- getLine
-- @
newtype Mut a = Mut a

infixl 0 :=
data MutAssign a b =
  -- | Used to assign a new value to a mutable variable. The assigned value
  -- is strictly evaluated.
  --
  -- @
  -- do
  -- let Mut x = True
  -- x := False
  -- @
  a := b
