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
__NOTE__: To prevent users to define `WithDict` instances, this module defines 'WithDict' the class and 'withDict' the function separately contrary to the "GHC.Magic.Dict".
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
