# ghc-magic-dict-compat - A compatibility layer and GHC Plugin for `withDict` magic function

Since GHC 9.4, the compiler provides magic type-class `WithDict` and its member function `withDict`:

```haskell
withDict :: 
  forall cls meth {rr} (r :: Type rr).
  WithDict cls meth =>
  meth ->
  (cls => r) ->
  r
```

This is the much safer version of `unsafeCoerce` to (unsafely) produce an instance dictionary for singleton classes dynamically - the compiler checks the preconditions statically at the compile time. Although it is potentially unsafe, this combinator is particularly useful when one writes the library manipulating instance dictionary dynamically.

This package provides a thin compatibility layer for `withDict` from `GHC.Magic.Dict` for GHC <9.4.
The package consists of the following two modules:

- `GHC.Magic.Dict.Compat`
- `GHC.Magic.Dict.Plugin`

All you have to do is to import `GHC.Magic.Dict.Compat` and put the following at the top of modules calling `withDict`:

```haskell
{-# OPTIONS_GHC -fplugin GHC.Magic.Dict.Plugin #-}
```

`GHC.Magic.Dict.Compat` provides a type-class `WithDict` and `withDict` combinator for GHC <9.4; it just re-expose them for GHC >= 9.4.
For GHC <9.4, user-facing API is almost the same, except for `withDict` is _not_ a member function of `WithDict`.
This is to prevent user-defined instances of `WithDict` by imposing unsolvable default signatures for hidden member functions.
Still, users can refer to `WithDict` the constraint and use `withDict` function almost the same way as in GHC >=9.4, so this subtle difference should not be a big problem.

As users cannot define the instance of `WithDict` manually, the `GHC.Magic.Dict.Compat` module alone is not enough.
Here, the GHC Plugin `GHC.Magic.Dict.Plugin` comes into play. For GHC <9.4, the plugin generates the dictionary for `WithDict` dynamically at the compile time, employing the almost the same logic as GHC >= 9.4. For newer GHC, it does no-op.

## Usage

1. Import `withDict` (and `WithDict` if necessary) from `GHC.Magic.Dict.Compat`
2. Enable GHC Plugin `GHC.Magic.Dict.Plugin` either by placing `{-# GHC_OPTIONS -fplugin GHC.Magic.Dict.Plugin #-}` at the top or adding `-fplugin GHC.Magic.Dict.Plugin` to `ghc-options` of the package.

With this, you can freely use `withDict` both with GHC <9.4 and >=9.4.

### Example

```hs
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, ConstraintKinds #-}
{-# GHC_OPTIONS -fplugin GHC.Magic.Dict.Plugin #-}
module MyModule where
import GHC.Magic.Dict.Compat

class Given a where
  given :: a

give :: a -> (Given a => r) -> r
give = withDict @(Given a) @a
```
