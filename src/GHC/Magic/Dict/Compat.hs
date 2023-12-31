{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
This module provides a compatibility layer of 'withDict' and 'WithDict' for GHC <9.4.
For GHC \<9.4, the definitions of 'WithDict' and 'withDict' are slightly different from those of GHC \>= 9.4 to prevent user-defined instances.

To actually make 'withDict' work, you have to invoke the accompanying GHC Plugin exposed from "GHC.Magic.Dict.Plugin". For example:

@
{\-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, ConstraintKinds #-\}
{\-# GHC_OPTIONS -fplugin "GHC.Magic.Dict.Plugin" #-\}
module MyModule where
import "GHC.Magic.Dict.Compat"

class Given a where
  given :: a

give :: a -> (Given a => r) -> r
give = 'withDict' \@(Given a) \@a
@

For GHC \>=9.4, this module just re-exports the module "GHC.Magic.Dict" and the plugin is just a no-op - so you can safely use this package without concerning break anything in newer GHCs.
-}
module GHC.Magic.Dict.Compat (
  WithDict,
  withDict,
) where

#if MIN_VERSION_ghc_prim(0,9,0)
import GHC.Magic.Dict (WithDict(), withDict)
#else

import GHC.TypeError.Compat (Unsatisfiable, unsatisfiable)
import GHC.TypeLits
import GHC.Types (RuntimeRep, TYPE)

class WithDict cls meth where
  withDict_ :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> ((cls) => r) -> r
  default withDict_ ::
    ( Unsatisfiable
        ( 'Text "Class `"
            ':<>: 'ShowType WithDict
            ':<>: 'Text " does not support user-specified-instances."
            ':$$: 'Text "In the instance declaration for `"
            ':<>: 'ShowType (WithDict cls meth)
            ':<>: 'Text "'"
        )
    ) =>
    forall {rr :: RuntimeRep} (r :: TYPE rr).
    meth ->
    ((cls) => r) ->
    r
  withDict_ = unsatisfiable

-- | @'withDict' d f@ provides a way to call a type-class–overloaded function
--   @f@ by applying it to the supplied dictionary @d@.
--
--   'withDict' can only be used if the type class has a single method with no
--   superclasses. For more (important) details on how this works, see
--   @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
withDict ::
  forall cls meth {rr :: RuntimeRep} (r :: TYPE rr).
  (WithDict cls meth) =>
  meth ->
  ((cls) => r) ->
  r
{-# INLINE withDict #-}
withDict = withDict_ @cls @meth
#endif
