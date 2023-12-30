{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
__NOTE__: to prevent users to define `WithDict` instances, this module DOES NOT expose 'WithDict' class itself, contrary to the "GHC.Magic.Dict".
-}
module GHC.Magic.Dict.Compat (
  withDict,
) where

#if MIN_VERSION_ghc_prim(0,9,0)
import GHC.Magic.Dict (withDict)
#else

import GHC.Types (RuntimeRep, TYPE)

class WithDict cls meth where
  {- |
  @'withDict' d f@ provides a way to call a type-class–overloaded function
  @f@ by applying it to the supplied dictionary @d@.

  'withDict' can only be used if the type class has a single method with no
  superclasses. For more (important) details on how this works, see
  @Note [withDict]@ in "GHC.Tc.Instance.Class" in GHC.
  -}
  withDict :: forall {rr :: RuntimeRep} (r :: TYPE rr). meth -> (cls => r) -> r
#endif
