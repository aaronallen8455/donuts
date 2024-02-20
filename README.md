# donuts

This is a GHC plugin that enables imperative style loops, mutable variables,
and early returns within any monadic do-block. The approach used is heavily
inspired by the do block features of Lean 4 and
[this related paper](https://dl.acm.org/doi/pdf/10.1145/3547640).

To use it, add this package as a dependency and add
`{-# OPTIONS_GHC -fplugin Donuts.Plugin #-}` to the top of a Haskell file and
import the `Donuts` module.

### Examples

Use `Mut` to bind a mutable variable and `:=` to assign it to a new value. The
mutability is scoped to the do block it is defined in as well as any nested
loop or `when` bodies, if statements, and case statements within that block.

```haskell
mutability :: Int
mutability = runIdentity $ do
  let Mut x = 5
  x := x * 2
  pure x

mutBind :: Maybe Int
mutBind = do
  Mut x <- Just 22
  x := 1
  guard (odd x)
  pure x
```

`earlyReturn` lets you short circuit a monadic computation, returning a given value.

```haskell
secretNum :: IO Int
secretNum = do
  inp <- getLine
  when (inp == "secret") (earlyReturn 42)
  putStrLn "Invalid password"
  pure 0
```

`forL` provides imperative style for-loops over any `Foldable` container. There
is also `whileL` for while-loops and `repeatL` for indefinitely repeating loops.

Within loop bodies, `continueL` and `breakL` provide the functionality
of their counterparts from imperative languages.

```haskell
skipEven :: IO ()
skipEven = do
  let i = 20 :: Int
  forL [1..i] $ \j -> do
    when (even j) continueL
    print j

countdown :: IO ()
countdown = do
  let Mut x = 5
  whileL (x > 0) $ do
    print x
    x := x - 1

loopBreak :: IO ()
loopBreak = do
  let Mut strings = []
  repeatL $ do
    inp <- getLine
    when (inp == "stop") breakL
    strings := inp : strings
    print strings
```
