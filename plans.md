For mutable variables, look for statements like
let mut foo = 5
within a do block and replace with essentially
(`runStateT` 5) $ do
and anywhere the variable is referenced, bind out the current state value in the prior statement
let blah = foo + 1
becomes
foo1 <- get
let blah = foo1 + 1
And also syntax for updating a mutable var:
foo := 8

A primary challenge will be with managing multiple mutable vars within in the
same scope. Could have a separate StateT layer for each variable and track the
stack depth for each one so that the correct number of lifts can be inserted.
May also be possible to have a single state layer with a dependent map. That
would probably involve fancy types that GHC would need to be able to infer,
which I don't have high confidence in.
The multiple state layer approach seems like it would probably work out.
Need to traverse the statements from top to bottom and once a state layer is
opened, find all the references for that variable and insert the corresponding
binds and well as lifting all other binds within the remaining statements. Then
continue looking through statements for mut decls and if one is found a new state
layer is created and all binds within the new block are lifted, included any
binds that were inserted for prior mutables vars.
Could have a state with a Nat index and different types at each index.
getVar (That (That Here))

data VarState idx where
  Nil :: VarState ()
  Var :: a -> VarState idx -> VarState (a, idx)
involves fancy types which might mean needed certain extensions on to type check
Also the state monad API doesn't have anything that would make extending the state
within a scope easy. It seems like a bit messy to try to do that.

A forseeable issue is that if the user puts a type signature on any statement
that is affected by the plugin, then the plugin will have to either remove it
or modify it to include whatever transformers are in play. This could be an issue
because the plugin won't know what the type of mutable variables should be. Could
use a wildcard for the state variable but that would force turning on partial type sigs.
Could use a "meta" type variable that is identifiable in some way and then the
plugin also includes a type checker plugin that unifies those variables with
whatever GHC thinks it should be.
^ Not actually an issue, just apply lift the whole expression, including the user's type sig.

Perhaps just cheat and use a dynamically typed hashmap to store mut vars. That
solves the issue of needing multiple StateT layers and also type sigs. There
is a problem with scoping though. need to remove vars that become out of scope
and also the potentially for name shadowing.

should mut vars be in scope for a bind that is itself a do block?
bar <- do
  pure foo
nah

For loops:
will need a keyword for loops, perhaps
forLoop xs $ \x -> do
or maybe just reuse the existing `for` combinator.
This will have some special characteristics:
- mutable variables declared before the loop are in scope in the loop body despite
  it being a different do block.
- interaction with continue and break statements (which only work if the value
  produced at each iteration is m ()).
  Perhaps the forLoop type should be restricted to loops that return unit.
Continue and break would make use of an ExceptT layer which can throw "exceptions"
that either break of the loop completely or go on to the next iteration.
This might entail two layers of ExceptT or having a custom implementation such as
forLoop [] _ = pure ()
forLoop (x : xs) f = do
  r <- runExceptT $ f x
  case r of
    Left Break -> pure ()
    _ -> forLoop xs f

also, whileLoop and repeatLoop
whileLoop could be tricky because it will need to be aware if there are mut
vars in the predicate and to refresh them on each check rather than binding
in preceding statement like normal.


A good starting point could be to implement the 'return' function. Will need a
different name for it, 'earlyReturn'?
Could implement in conjunction with 'repeat' and 'for' loops
Would it be better to use ExceptT or ContT? ExceptT seems simpler

Implementation plan for earlyReturn:
- Look for occurences of do blocks
- look through all statements checking for presence of 'earlyReturn'
- If any are found, insert a function before the do block that wraps it in
  ExceptT and handles the early return case.
- Insert a `lift` before all monadic binds in the do block
- If any monadic binds have a type sig, at the ExceptT constructor to the result type.

Would it better to use a renamer or parser plugin? Using renamer might ease some
of the scoping issues in that the renamer will handle shadowing by assigning a
different name. Not very high value but might be somewhat useful for working with
mut vars. Renamer result will have certain transformations such as applicative do
which might cause some issues but shouldn't be a driving factor. Going with renamer
to make things potentially slightly easier although parser seems viable as well.
