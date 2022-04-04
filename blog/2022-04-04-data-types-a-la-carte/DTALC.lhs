I am reviewing (making sure I understand) some classic Haskell papers.  The
first up is [Wouter Swierstra][]'s [Data types à la carte][] ([CiteSeer][]),
first published in 2008.  Inspired by [Sandy Maguire][], I am writing this
blog entry as part of my review.

[Wouter Swierstra]: <https://webspace.science.uu.nl/~swier004/>
[Data types à la carte]: <http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf>
[CiteSeer]: <https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131>
[Sandy Maguire]: <https://reasonablypolymorphic.com/>

This blog entry is written in [Literate Haskell][], using [LiterateX][] to
render the HTML.  The [source code][] is available on GitHub.  Note that the
code is in the order used in the paper.

[Literate Haskell]: <https://wiki.haskell.org/Literate_programming>
[LiterateX]: <https://github.com/ExtremaIS/literatex-haskell#literatex>
[source code]: <https://github.com/ExtremaIS/www.extrema.is-code/tree/main/blog/2022-04-04-data-types-a-la-carte>

Setup
-----

This implementation uses the following GHC extensions.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeOperators #-}

It is implemented as a library so that it is easy to experiment with using
GHCi.  As a convention, functions with names *starting with* `main` have type
`IO ()`, providing an easy way to run various tests from within GHCi.

> module DTALC where

The following imports are used.

> -- https://hackage.haskell.org/package/base
> import Control.Applicative ((<|>))
> import qualified Prelude
> import Prelude hiding (getChar, putChar, readFile, writeFile)

Fixing the expression problem
-----------------------------

This section shows how to represent expressions without using a sum type that
specifies all valid constructors.  Being able to separate/modularize the parts
of an expression resolves the famous "[expression problem][]."

[expression problem]: <https://en.wikipedia.org/wiki/Expression_problem>

The following defines a recursive data type to represent expressions.  Type
parameter `f` specifies the type constructor(s) that can be used in an
expression.  It takes a type parameter that specifies the type of the
expression (`Expr f`), used in the recursive parts.

> newtype Expr f = In (f (Expr f))

Perhaps it is worthwhile to compare this definition to the `Expr` definition
in the paper introduction.  In the introduction, `Expr` is a sum type that
specifies all valid (value) constructors.  The `Add` constructor has recursive
arguments, allowing the addition of expressions (not just values).  In the
above definition, type `f` can be used to specify all valid types, using
*separate* types instead of a single sum type.  On the right-hand-side,
`f (Expr f)` specifies that a value of `Expr f` is a value of one of the types
specified by "signature" `f`, with recursive parts of type `Expr f`.  A
`newtype` is required for the type system, using `In` to construct values of
`Expr`.

The `Val` and `Add` types are given as initial examples.  The `IntExpr`and
`AddExpr` shows how these can be used with the above `Expr` type, but note
that they are not otherwise used.

> newtype Val e = Val Int
> type IntExpr = Expr Val
>
> data Add e = Add e e
> type AddExpr = Expr Add

The following defines a type-level operator that represents the coproduct of
two signatures.  It requires the [TypeOperators][] extension.  It is used like
a list constructed from [cons][] pairs, with left cells indicating type
constructors and right cells pointing to the next pair, except that the chain
ends with the final right cell pointing to a type constructor instead of null.
The operator is marked as right-associative so that these chains are
constructed correctly.  There are better ways to implement this, but they are
not discussed in this paper.

[TypeOperators]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_operators.html>
[cons]: <https://en.wikipedia.org/wiki/Cons>

> data (f :+: g) e = Inl (f e) | Inr (g e)
> infixr 8 :+:

This type operator is infix, but it has an additional type parameter `e`.
Note that `(f :+: g) e` could alternatively be written `(:+:) f g e`.

The following `addExample` definition illustrates how bad it would be to
create expressions using just the above definitions.

> addExample :: Expr (Val :+: Add)
> addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

Evaluation
----------

This section shows how to evaluate expressions.

The [`Functor`][] definitions are straightforward.  Note that I use the
[InstanceSigs][] extension to explicitly write the specialized signatures for
all instance methods in this implementation.  Though it is likely unhelpful in
this case, such explicit signatures can help understand more complex
definitions.  I also avoid deriving any instances so that the definitions are
explicit.

[`Functor`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Data-Functor.html>
[InstanceSigs]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-InstanceSigs>

> instance Functor Val where
>   fmap :: (a -> b) -> Val a -> Val b
>   fmap _f (Val x) = Val x
>
> instance Functor Add where
>   fmap :: (a -> b) -> Add a -> Add b
>   fmap f (Add x y) = Add (f x) (f y)
>
> instance (Functor f, Functor g) => Functor (f :+: g) where
>   fmap :: (a -> b) -> (f :+: g) a -> (f :+: g) b
>   fmap f = \case
>     Inl e -> Inl (fmap f e)
>     Inr e -> Inr (fmap f e)

Note that there are a few (unimportant) stylistic changes in the above code.
I use `_f` in the `Val` instance to indicate that the function is not used in
the definition.  I also use the [LambdaCase][] extension.

[LambdaCase]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html#extension-LambdaCase>

The `foldExpr` function folds an expression using the provided "algebra."  It
uses the `Functor` instances to traverse the recursive parts.  Perhaps due to
my [Scheme][] background, I prefer to capture the first argument in a closure
and use a helper function for the recursion.  Since the type signature for the
helper function references type variables in the type signature of `foldExpr`,
the [ScopedTypeVariables][] extension is required.

[Scheme]: <https://en.wikipedia.org/wiki/Scheme_(programming_language)>
[ScopedTypeVariables]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables>

> foldExpr :: forall f a. Functor f => (f a -> a) -> Expr f -> a
> foldExpr f = go
>   where
>     go :: Expr f -> a
>     go (In t) = f (fmap go t)

An algebra is defined for evaluating expressions.  The use of a type class
allows evaluation of each part of an expression to be defined separately.

> class Functor f => Eval f where
>   evalAlgebra :: f Int -> Int

The instances are straighforward.  The type signatures clearly show that the
type parameters are `Int` when performing evaluation (*not* `Expr f`).  This
illustrates the versatility of the abstract definitions.

> instance Eval Val where
>   evalAlgebra :: Val Int -> Int
>   evalAlgebra (Val x) = x
>
> instance Eval Add where
>   evalAlgebra :: Add Int -> Int
>   evalAlgebra (Add x y) = x + y
>
> instance (Eval f, Eval g) => Eval (f :+: g) where
>   evalAlgebra :: (f :+: g) Int -> Int
>   evalAlgebra = \case
>     Inl x -> evalAlgebra x
>     Inr y -> evalAlgebra y

The `eval` function implements evaluation using `foldExpr` and `evalAlgebra`.

> eval :: Eval f => Expr f -> Int
> eval = foldExpr evalAlgebra

The example evaluates `addExample`.

> mainEvalAddExample :: IO ()
> mainEvalAddExample = print $ eval addExample

Automating injections
---------------------

This section shows how to avoid having to manually write out all of the
constructors like was done in `addExample`.  A type class is used to define a
constraint that asserts that a specific type is a member of a signature.  The
`inj` method represents [injection][], while the `prj` method discussed at the
end of the next section represents the partial inverse.

[injection]: <https://ncatlab.org/nlab/show/injection>

> class (Functor sub, Functor sup) => sub :<: sup where
>   inj :: sub a -> sup a
>   prj :: sup a -> Maybe (sub a)

Since this class has two parameters, the [MultiParamTypeClasses][] extension
is required.

[MultiParamTypeClasses]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multi_param_type_classes.html>

The instances are very straightforward.  Note that the [FlexibleInstances][]
extension is required because `f` is not a distinct type variable.  The paper
notes the overlapping instances, and the final instance needs to be annotated
with `OVERLAPPABLE` to compile.

[FlexibleInstances]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-FlexibleInstances>

> instance Functor f => f :<: f where
>   inj :: f a -> f a
>   inj = id
>
>   prj :: f a -> Maybe (f a)
>   prj = Just
>
> instance (Functor f, Functor g) => f :<: (f :+: g) where
>   inj :: f a -> (f :+: g) a
>   inj = Inl
>
>   prj :: (f :+: g) a -> Maybe (f a)
>   prj = \case
>     Inl e  -> Just e
>     Inr _e -> Nothing
>
> instance {-# OVERLAPPABLE #-}
>   (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
>     inj :: f a -> (h :+: g) a
>     inj = Inr . inj
>
>     prj :: (h :+: g) a -> Maybe (f a)
>     prj = \case
>       Inl _e -> Nothing
>       Inr e  -> prj e

The smart constructors are implemented using an `inject` helper function.  The
[FlexibleContexts][] extension is required because non-type-variable arguments
are used in the constraint.  Note that I implement the smart constructor for
`Add` as `%+%` instead of using Unicode.

[FlexibleContexts]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts>

> inject :: (g :<: f) => g (Expr f) -> Expr f
> inject = In . inj
>
> val :: (Val :<: f) => Int -> Expr f
> val x = inject (Val x)
>
> (%+%) :: (Add :<: f) => Expr f -> Expr f -> Expr f
> x %+% y = inject (Add x y)
> infixl 6 %+%

These smart constructors are much more convenient than the constructors used
in `addExample`!

> addExample2 :: Expr (Val :+: Add)
> addExample2 = val 30000 %+% val 1330 %+% val 7
>
> mainEvalAddExample2 :: IO ()
> mainEvalAddExample2 = print $ eval addExample2

Examples
--------

This section shows how new expression terms and functions can be added without
having to change the implementation of existing terms and functions.

Multiplication is implemented as `Mul`.  Note that I implemented the smart
constructor for `Mul` as `%*%` instead of using Unicode.

> data Mul e = Mul e e
>
> instance Functor Mul where
>   fmap :: (a -> b) -> Mul a -> Mul b
>   fmap f (Mul x y) = Mul (f x) (f y)
>
> instance Eval Mul where
>   evalAlgebra :: Mul Int -> Int
>   evalAlgebra (Mul x y) = x * y
>
> (%*%) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
> x %*% y = inject (Mul x y)
> infixl 7 %*%

The examples demonstrate how this new term can be used with the existing
terms.

> mulExample :: Expr (Val :+: Add :+: Mul)
> mulExample = val 80 %*% val 5 %+% val 4
>
> mainEvalMulExample :: IO ()
> mainEvalMulExample = print $ eval mulExample
>
> mulExample2 :: Expr (Val :+: Mul)
> mulExample2 = val 6 %*% val 7
>
> mainEvalMulExample2 :: IO ()
> mainEvalMulExample2 = print $ eval mulExample2

Pretty printing is implemented using the `Render` type class.

> class Render f where
>   render :: Render g => f (Expr g) -> String
>
> pretty :: Render f => Expr f -> String
> pretty (In t) = render t

Like evaluation, instances are defined for each term as well as `(:+:)`.

> instance Render Val where
>   render :: Render g => Val (Expr g) -> String
>   render (Val i) = show i
>
> instance Render Add where
>   render :: Render g => Add (Expr g) -> String
>   render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
>
> instance Render Mul where
>   render :: Render g => Mul (Expr g) -> String
>   render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
>
> instance (Render f, Render g) => Render (f :+: g) where
>   render :: Render h => (f :+: g) (Expr h) -> String
>   render = \case
>     Inl x -> render x
>     Inr y -> render y

I implement a `Show` instance for convenience.

> instance Render f => Show (Expr f) where
>   show :: Expr f -> String
>   show = pretty

The example pretty-prints `mulExample`.

> mainPrettyMulExample :: IO ()
> mainPrettyMulExample = print mulExample

I think that the discussion about `prj` is very important.  When using a sum
type, one can pattern-match against all of the constructors of that type, but
this is not possible using `Expr`.  This discussion shows how to use `prj` to
match constructors, specifying the appropriate type constraints.  The `match`
function simply unwraps the `Expr` constructor and calls `prj` on the wrapped
value.

> match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
> match (In t) = prj t

The `distr` function applies the [distributive law][] on the outermost
constructors of an expression.  It is used for [term rewriting][].  As shown,
however, it only works when the right multiplicand is an addition.  It does
not work when the left multiplicand is an addition.  The following version
supports both.

[distributive law]: <https://en.wikipedia.org/wiki/Distributive_property>
[term rewriting]: <https://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems>

> distr :: forall f. (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
> distr t = distrL <|> distrR
>   where
>     distrL :: Maybe (Expr f)
>     distrL = do
>       Mul a b <- match t
>       Add c d <- match b
>       return (a %*% c %+% a %*% d)
>
>     distrR :: Maybe (Expr f)
>     distrR = do
>       Mul a b <- match t
>       Add c d <- match a
>       return (c %*% b %+% d %*% b)

The paper states that an algebra can be defined to fold over an expression to
uniformly apply the distributive law using `distr`, but no code is provided.
I tried implementing this myself, but I do not yet know how to write an
algebra (type `f a -> a`) to do this.  A type class should not be required, as
no new functionality needs to be implemented per term.  Also, it cannot be
done in a single pass because one rewrite may enable further rewrites in
sub-expressions.

I *tried* implementing this using the following function, which uses
mutually-recursive helper functions.

> rewriteExpr
>   :: forall f. Functor f
>   => (Expr f -> Maybe (Expr f)) -> Expr f -> Expr f
> rewriteExpr f = go
>   where
>     go :: Expr f -> Expr f
>     go (In t) = f' (In (fmap go t))
>
>     f' :: Expr f -> Expr f
>     f' e = maybe e go (f e)

This function works fine when using GHC 8.10.7, but compilation fails when
using GHC 9.0.2 with a "simplifier ticks exhausted."  The error persists when
I replace `f'` with `id`, so the problem is with the recursion in the `go`
helper function.  It is very similar to the `foldExpr` definition, except that
it returns expressions instead of folded values.  Simplifying, even definition
`rewriteExpr f (In t) = In (fmap (rewriteExpr f) t)` causes the error.

Using GHC 8.10.7, the `rewriteDistr` function is a helper function that uses
`rewriteExpr` to rewrite an expression using just the `distr` function.

> rewriteDistr :: (Add :<: f, Mul :<: f) => Expr f -> Expr f
> rewriteDistr = rewriteExpr distr

The following are examples of incresting complexity.

> distrExample1 :: Expr (Val :+: Add :+: Mul)
> distrExample1 = val 2 %*% (val 3 %+% val 4)
>
> mainDistrExample1 :: IO ()
> mainDistrExample1 = do
>     print distrExample1
>     print $ rewriteDistr distrExample1

> distrExample2 :: Expr (Val :+: Add :+: Mul)
> distrExample2 = (val 2 %+% val 3) %*% val 4
>
> mainDistrExample2 :: IO ()
> mainDistrExample2 = do
>     print distrExample2
>     print $ rewriteDistr distrExample2

> distrExample3 :: Expr (Val :+: Add :+: Mul)
> distrExample3 = val 2  %*% ((val 3 %+% val 4) %*% val 5)
>
> mainDistrExample3 :: IO ()
> mainDistrExample3 = do
>     print distrExample3
>     print $ rewriteDistr distrExample3

> distrExample4 :: Expr (Val :+: Add :+: Mul)
> distrExample4 = (val 2 %+% val 3) %*% (val 4 %*% (val 5 %+% val 6))
>
> mainDistrExample4 :: IO ()
> mainDistrExample4 = do
>     print distrExample4
>     print $ rewriteDistr distrExample4

> distrExample5 :: Expr (Val :+: Add :+: Mul)
> distrExample5 =
>   (val 2 %*% (val 3 %+% val 4)) %*%
>   ((val 5 %+% val 6) %*% (val 7 %*% (val 8 %+% val 9)))
>
> mainDistrExample5 :: IO ()
> mainDistrExample5 = do
>     print distrExample5
>     print $ rewriteDistr distrExample5

All of the rewritten expressions are indeed in [disjunctive normal form][],
but I am very interested in finding a better solution.  If you know of a
different solution or how to fix `rewriteExpr` to work with GHC 9, please let
me know!

[disjunctive normal form]: <https://en.wikipedia.org/wiki/Disjunctive_normal_form>

Monads for free
---------------

This section shows how a [free monad][] is implemented in Haskell.  It is used
to implement a calculator memory cell.

[free monad]: <https://ncatlab.org/nlab/show/free+monad>

A free monad is implemented as type `Term`.  Note that an [`Applicative`][]
instance is now required for defining a [`Monad`][] instance.

[`Applicative`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Control-Applicative.html#t:Applicative>
[`Monad`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Control-Monad.html#t:Monad>

> data Term f a
>   = Pure a
>   | Impure (f (Term f a))
>
> instance Functor f => Functor (Term f) where
>   fmap :: (a -> b) -> Term f a -> Term f b
>   fmap f = \case
>     Pure x   -> Pure (f x)
>     Impure t -> Impure (fmap (fmap f) t)
>
> instance Functor f => Applicative (Term f) where
>   pure :: a -> Term f a
>   pure x = Pure x
>
>   (<*>) :: Term f (a -> b) -> Term f a -> Term f b
>   Pure f   <*> t = fmap f t
>   Impure f <*> t = Impure (fmap (<*> t) f)
>
> instance Functor f => Monad (Term f) where
>   return :: a -> Term f a
>   return x = Pure x
>
>   (>>=) :: Term f a -> (a -> Term f b) -> Term f b
>   Pure x   >>= f = f x
>   Impure t >>= f = Impure (fmap (>>= f) t)

The following examples are not used in the implementation.

> data Zero a
>
> data One a = One
>
> newtype Const e a = Const e

The `Incr` and `Recall` types represent the "increment" and "recall"
operations.  Note that "increment" is not used to mean the addition of one; it
represents the addition of an arbitrary value.  These types have
straightforward [`Functor`][] instances.

> data Incr t = Incr Int t
>
> instance Functor Incr where
>   fmap :: (a -> b) -> Incr a -> Incr b
>   fmap f (Incr i g) = Incr i (f g)
>
> newtype Recall t = Recall (Int -> t)
>
> instance Functor Recall where
>   fmap :: (a -> b) -> Recall a -> Recall b
>   fmap f (Recall g) = Recall (f . g)

The smart constructors are implemented using an `inject` helper function,
which I name `injectTerm` because there is already an `inject` function in
this module.

> injectTerm :: (g :<: f) => g (Term f a) -> Term f a
> injectTerm = Impure . inj
>
> incr :: (Incr :<: f) => Int -> Term f ()
> incr i = injectTerm (Incr i (Pure ()))
>
> recall :: (Recall :<: f) => Term f Int
> recall = injectTerm (Recall Pure)

The `tick` function really does increment a value (by one).  A note in the
paper states that it can be given a more general type, which I use here.

> tick :: (Recall :<: f, Incr :<: f) => Term f Int
> tick = do
>     y <- recall
>     incr 1
>     return y

While `foldExpr` folds an expression using an algebra, the `foldTerm` function
folds a `Term` using separate functions for handling `Pure` and `Impure`.
Note that I name the first argument `pure'` because [`pure`][] is a
[Prelude][] function.

[`pure`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Prelude.html#v:pure>
[Prelude]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Prelude.html>

> foldTerm
>   :: forall f a b. Functor f
>   => (a -> b) -> (f b -> b) -> Term f a -> b
> foldTerm pure' impure = go
>   where
>     go :: Term f a -> b
>     go = \case
>       Pure x   -> pure' x
>       Impure t -> impure (fmap go t)

The `Mem` type represents the memory cell of a calculator.

> newtype Mem = Mem Int
>   deriving Show

Like `Eval`, a `Run` type class defines an algebra for running the operations,
with implementations of each operation in instance definitions.

> class Functor f => Run f where
>   runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
>
> instance Run Incr where
>   runAlgebra :: Incr (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
>   runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))
>
> instance Run Recall where
>   runAlgebra :: Recall (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
>   runAlgebra (Recall r) (Mem i) = r i (Mem i)
>
> instance (Run f, Run g) => Run (f :+: g) where
>   runAlgebra = \case
>     Inl r -> runAlgebra r
>     Inr r -> runAlgebra r

I wrote the type signatures like in the paper, which makes it clear that the
member function is an algebra (type `f a -> a`), but note that the parenthesis
around `(Mem -> (a, Mem))` at the end of the signature are not necessary.  In
the instances for `Incr` and `Recall`, `runAlgebra` has *two* parameters.  The
first is `f (Mem -> (a, Mem))` and the second is `Mem`, giving a return value
of `(a, Mem)`.

The `run` function is perhaps the most difficult to understand in the paper.

> run :: Run f => Term f a -> Mem -> (a, Mem)
> run = foldTerm (,) runAlgebra

The `pure'` parameter of `foldTerm` has type `a -> b` and is passed `(,)`
which has type `a -> b -> (a, b)`.  In the usage of `foldTerm`, type parameter
`a` remains `a` while type parameter `b` is interpreted as `b -> (a, b)`,
resulting in `(f (b -> (a, b)) -> b -> (a, b)) -> Term f a -> b -> (a, b)`.
The `runAlgebra` function matches the first *three* arguments of this, and `b`
is interpreted as `Mem`, resulting in the type signature of `run`.

It is worthwhile to (mentally) step through the complete evaluation of `run`
with the following basic values of `Term` (`Pure`, `recall`, and `incr`).

> mainRunPure :: IO ()
> mainRunPure = print $ run @Recall @Int (Pure 42) (Mem 0)
>
> mainRunRecall :: IO ()
> mainRunRecall = print $ run @Recall @Int recall (Mem 42)
>
> mainRunIncr :: IO ()
> mainRunIncr = print $ run @Incr @() (incr 42) (Mem 0)

Note that the [TypeApplications][] extension is used to specify the type of
`f` and `a`.

[TypeApplications]: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#extension-TypeApplications>

An example using `tick` is given.  The paper does not require use of
[TypeApplications][] because it uses a specific type, while the following uses
[TypeApplications][] because the above definition of `tick` uses the more
general type.

> mainRunTick :: IO ()
> mainRunTick = print $ run @(Recall :+: Incr) tick (Mem 4)

Applications
------------

This section gives a brief demonstration of using free monads to model
effects.

Four effectful functions are defined, categorized into two separate data
types.

> data Teletype a
>   = GetChar (Char -> a)
>   | PutChar Char a
>
> instance Functor Teletype where
>   fmap :: (a -> b) -> Teletype a -> Teletype b
>   fmap f = \case
>     GetChar g   -> GetChar (f . g)
>     PutChar c g -> PutChar c (f g)
>
> data FileSystem a
>   = ReadFile FilePath (String -> a)
>   | WriteFile FilePath String a
>
> instance Functor FileSystem where
>   fmap :: (a -> b) -> FileSystem a -> FileSystem b
>   fmap f = \case
>     ReadFile path g    -> ReadFile path (f . g)
>     WriteFile path s g -> WriteFile path s (f g)

An `exec` function can execute values of these data types using the `Term`
free monad.

> exec :: Exec f => Term f a -> IO a
> exec = foldTerm return execAlgebra

Each value is executed using a function from [Prelude][], using an `Exec` type
class.

> class Functor f => Exec f where
>   execAlgebra :: f (IO a) -> IO a
>
> instance Exec Teletype where
>   execAlgebra = \case
>     GetChar f    -> Prelude.getChar >>= f
>     PutChar c io -> Prelude.putChar c >> io
>
> instance Exec FileSystem where
>   execAlgebra = \case
>     ReadFile path f    -> Prelude.readFile path >>= f
>     WriteFile path s f -> Prelude.writeFile path s >> f
>
> instance (Exec f, Exec g) => Exec (f :+: g) where
>   execAlgebra = \case
>     Inl e -> execAlgebra e
>     Inr e -> execAlgebra e

The smart constructors can be defined as follows.

> getChar :: (Teletype :<: f) => Term f Char
> getChar = injectTerm (GetChar Pure)
>
> putChar :: (Teletype :<: f) => Char -> Term f ()
> putChar c = injectTerm (PutChar c (Pure ()))
>
> readFile :: (FileSystem :<: f) => FilePath -> Term f String
> readFile path = injectTerm (ReadFile path Pure)
>
> writeFile :: (FileSystem :<: f) => FilePath -> String -> Term f ()
> writeFile path s = injectTerm (WriteFile path s (Pure ()))

The `cat` function serves as an example of composition.  In the following, I
use a more general type than that used in the paper.  I also refactored the
implementation, using [`mapM_`][] instead of [`mapM`][] to avoid discarding
the resulting list of unit.

[`mapM_`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Control-Monad.html#v:mapM_>
[`mapM`]: <https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.1.0/Control-Monad.html#v:mapM>

> cat :: (FileSystem :<: f, Teletype :<: f) => FilePath -> Term f ()
> cat path = mapM_ putChar =<< readFile path

The following example uses the `cat` function to print the content of the
`README.md` file in this directory.

> mainCat :: IO ()
> mainCat = exec @(FileSystem :+: Teletype) $ cat "README.md"
