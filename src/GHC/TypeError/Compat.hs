{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GHC.TypeError.Compat (Unsatisfiable, unsatisfiable) where

#if MIN_VERSION_base(4,19,0)
import GHC.TypeError  (Unsatisfiable, unsatisfiable)
#else
import Data.Void (Void)
import GHC.Exts
import GHC.TypeLits

class Any => Bottom where
  unsatisfiable' :: Void

class (Bottom, TypeError e) => Unsatisfiable e
instance (Bottom, TypeError e) => Unsatisfiable e

unsatisfiable :: forall {rep} (a :: TYPE rep). (Bottom) => a
unsatisfiable = case unsatisfiable' of {}
#endif
